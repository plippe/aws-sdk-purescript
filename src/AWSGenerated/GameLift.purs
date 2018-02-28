

-- | <fullname>Amazon GameLift Service</fullname> <p> Amazon GameLift is a managed service for developers who need a scalable, dedicated server solution for their multiplayer games. Use Amazon GameLift for these tasks: (1) set up computing resources and deploy your game servers, (2) run game sessions and get players into games, (3) automatically scale your resources to meet player demand and manage costs, and (4) track in-depth metrics on game server performance and player usage.</p> <p>The Amazon GameLift service API includes two important function sets:</p> <ul> <li> <p> <b>Manage game sessions and player access</b> -- Retrieve information on available game sessions; create new game sessions; send player requests to join a game session.</p> </li> <li> <p> <b>Configure and manage game server resources</b> -- Manage builds, fleets, queues, and aliases; set autoscaling policies; retrieve logs and metrics.</p> </li> </ul> <p>This reference guide describes the low-level service API for Amazon GameLift. You can use the API functionality with these tools: </p> <ul> <li> <p>The Amazon Web Services software development kit (<a href="http://aws.amazon.com/tools/#sdk">AWS SDK</a>) is available in <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-supported.html#gamelift-supported-clients">multiple languages</a> including C++ and C#. Use the SDK to access the API programmatically from an application, such as a game client.</p> </li> <li> <p>The <a href="http://aws.amazon.com/cli/">AWS command-line interface</a> (CLI) tool is primarily useful for handling administrative actions, such as setting up and managing Amazon GameLift settings and resources. You can use the AWS CLI to manage all of your AWS services.</p> </li> <li> <p>The <a href="https://console.aws.amazon.com/gamelift/home">AWS Management Console</a> for Amazon GameLift provides a web interface to manage your Amazon GameLift settings and resources. The console includes a dashboard for tracking key resources, including builds and fleets, and displays usage and performance metrics for your games as customizable graphs.</p> </li> <li> <p>Amazon GameLift Local is a tool for testing your game's integration with Amazon GameLift before deploying it on the service. This tools supports a subset of key API actions, which can be called from either the AWS CLI or programmatically. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/integration-testing-local.html">Testing an Integration</a>.</p> </li> </ul> <p> <b>Learn more</b> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/"> Developer Guide</a> -- Read about Amazon GameLift features and how to use them. </p> </li> <li> <p> <a href="https://gamedev.amazon.com/forums/tutorials">Tutorials</a> -- Get started fast with walkthroughs and sample projects.</p> </li> <li> <p> <a href="http://aws.amazon.com/blogs/gamedev/">GameDev Blog</a> -- Stay up to date with new features and techniques.</p> </li> <li> <p> <a href="https://gamedev.amazon.com/forums/spaces/123/gamelift-discussion.html">GameDev Forums</a> -- Connect with the GameDev community.</p> </li> <li> <p> <a href="http://aws.amazon.com/releasenotes/Amazon-GameLift/">Release notes</a> and <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/doc-history.html">document history</a> -- Stay current with updates to the Amazon GameLift service, SDKs, and documentation. </p> </li> </ul> <p> <b>API SUMMARY</b> </p> <p>This list offers a functional overview of the Amazon GameLift service API.</p> <p> <b>Managing Games and Players</b> </p> <p>Use these actions to start new game sessions, find existing game sessions, track game session status and other information, and enable player access to game sessions.</p> <ul> <li> <p> <b>Discover existing game sessions</b> </p> <ul> <li> <p> <a>SearchGameSessions</a> -- Retrieve all available game sessions or search for game sessions that match a set of criteria. </p> </li> </ul> </li> <li> <p> <b>Start new game sessions</b> </p> <ul> <li> <p>Start new games with Queues to find the best available hosting resources across multiple regions, minimize player latency, and balance game session activity for efficiency and cost effectiveness. </p> <ul> <li> <p> <a>StartGameSessionPlacement</a> -- Request a new game session placement and add one or more players to it.</p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> -- Get details on a placement request, including status.</p> </li> <li> <p> <a>StopGameSessionPlacement</a> -- Cancel a placement request. </p> </li> </ul> </li> <li> <p> <a>CreateGameSession</a> -- Start a new game session on a specific fleet. <i>Available in Amazon GameLift Local.</i> </p> </li> </ul> </li> <li> <p> <b>Match players to game sessions with FlexMatch matchmaking</b> </p> <ul> <li> <p> <a>StartMatchmaking</a> -- Request matchmaking for one players or a group who want to play together. </p> </li> <li> <p> <a>StartMatchBackfill</a> - Request additional player matches to fill empty slots in an existing game session. </p> </li> <li> <p> <a>DescribeMatchmaking</a> -- Get details on a matchmaking request, including status.</p> </li> <li> <p> <a>AcceptMatch</a> -- Register that a player accepts a proposed match, for matches that require player acceptance. </p> </li> <li> <p> <a>StopMatchmaking</a> -- Cancel a matchmaking request. </p> </li> </ul> </li> <li> <p> <b>Manage game session data</b> </p> <ul> <li> <p> <a>DescribeGameSessions</a> -- Retrieve metadata for one or more game sessions, including length of time active and current player count. <i>Available in Amazon GameLift Local.</i> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> -- Retrieve metadata and the game session protection setting for one or more game sessions.</p> </li> <li> <p> <a>UpdateGameSession</a> -- Change game session settings, such as maximum player count and join policy.</p> </li> <li> <p> <a>GetGameSessionLogUrl</a> -- Get the location of saved logs for a game session.</p> </li> </ul> </li> <li> <p> <b>Manage player sessions</b> </p> <ul> <li> <p> <a>CreatePlayerSession</a> -- Send a request for a player to join a game session. <i>Available in Amazon GameLift Local.</i> </p> </li> <li> <p> <a>CreatePlayerSessions</a> -- Send a request for multiple players to join a game session. <i>Available in Amazon GameLift Local.</i> </p> </li> <li> <p> <a>DescribePlayerSessions</a> -- Get details on player activity, including status, playing time, and player data. <i>Available in Amazon GameLift Local.</i> </p> </li> </ul> </li> </ul> <p> <b>Setting Up and Managing Game Servers</b> </p> <p>When setting up Amazon GameLift resources for your game, you first <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html">create a game build</a> and upload it to Amazon GameLift. You can then use these actions to configure and manage a fleet of resources to run your game servers, scale capacity to meet player demand, access performance and utilization metrics, and more.</p> <ul> <li> <p> <b>Manage game builds</b> </p> <ul> <li> <p> <a>CreateBuild</a> -- Create a new build using files stored in an Amazon S3 bucket. To create a build and upload files from a local path, use the AWS CLI command <code>upload-build</code>.</p> </li> <li> <p> <a>ListBuilds</a> -- Get a list of all builds uploaded to a Amazon GameLift region.</p> </li> <li> <p> <a>DescribeBuild</a> -- Retrieve information associated with a build.</p> </li> <li> <p> <a>UpdateBuild</a> -- Change build metadata, including build name and version.</p> </li> <li> <p> <a>DeleteBuild</a> -- Remove a build from Amazon GameLift.</p> </li> </ul> </li> <li> <p> <b>Manage fleets</b> </p> <ul> <li> <p> <a>CreateFleet</a> -- Configure and activate a new fleet to run a build's game servers.</p> </li> <li> <p> <a>ListFleets</a> -- Get a list of all fleet IDs in a Amazon GameLift region (all statuses).</p> </li> <li> <p> <a>DeleteFleet</a> -- Terminate a fleet that is no longer running game servers or hosting players.</p> </li> <li> <p>View / update fleet configurations.</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> / <a>UpdateFleetAttributes</a> -- View or change a fleet's metadata and settings for game session protection and resource creation limits.</p> </li> <li> <p> <a>DescribeFleetPortSettings</a> / <a>UpdateFleetPortSettings</a> -- View or change the inbound permissions (IP address and port setting ranges) allowed for a fleet.</p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> / <a>UpdateRuntimeConfiguration</a> -- View or change what server processes (and how many) to run on each instance in a fleet.</p> </li> </ul> </li> </ul> </li> <li> <p> <b>Control fleet capacity</b> </p> <ul> <li> <p> <a>DescribeEC2InstanceLimits</a> -- Retrieve maximum number of instances allowed for the current AWS account and the current usage level.</p> </li> <li> <p> <a>DescribeFleetCapacity</a> / <a>UpdateFleetCapacity</a> -- Retrieve the capacity settings and the current number of instances in a fleet; adjust fleet capacity settings to scale up or down.</p> </li> <li> <p>Autoscale -- Manage autoscaling rules and apply them to a fleet.</p> <ul> <li> <p> <a>PutScalingPolicy</a> -- Create a new autoscaling policy, or update an existing one.</p> </li> <li> <p> <a>DescribeScalingPolicies</a> -- Retrieve an existing autoscaling policy.</p> </li> <li> <p> <a>DeleteScalingPolicy</a> -- Delete an autoscaling policy and stop it from affecting a fleet's capacity.</p> </li> </ul> </li> </ul> </li> <li> <p> <b>Manage VPC peering connections for fleets</b> </p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> -- Authorize a peering connection to one of your VPCs.</p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> -- Retrieve valid peering connection authorizations. </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> -- Delete a peering connection authorization.</p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> -- Establish a peering connection between the VPC for a Amazon GameLift fleet and one of your VPCs.</p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> -- Retrieve information on active or pending VPC peering connections with a Amazon GameLift fleet.</p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> -- Delete a VPC peering connection with a Amazon GameLift fleet.</p> </li> </ul> </li> <li> <p> <b>Access fleet activity statistics</b> </p> <ul> <li> <p> <a>DescribeFleetUtilization</a> -- Get current data on the number of server processes, game sessions, and players currently active on a fleet.</p> </li> <li> <p> <a>DescribeFleetEvents</a> -- Get a fleet's logged events for a specified time span.</p> </li> <li> <p> <a>DescribeGameSessions</a> -- Retrieve metadata associated with one or more game sessions, including length of time active and current player count.</p> </li> </ul> </li> <li> <p> <b>Remotely access an instance</b> </p> <ul> <li> <p> <a>DescribeInstances</a> -- Get information on each instance in a fleet, including instance ID, IP address, and status.</p> </li> <li> <p> <a>GetInstanceAccess</a> -- Request access credentials needed to remotely connect to a specified instance in a fleet.</p> </li> </ul> </li> <li> <p> <b>Manage fleet aliases</b> </p> <ul> <li> <p> <a>CreateAlias</a> -- Define a new alias and optionally assign it to a fleet.</p> </li> <li> <p> <a>ListAliases</a> -- Get all fleet aliases defined in a Amazon GameLift region.</p> </li> <li> <p> <a>DescribeAlias</a> -- Retrieve information on an existing alias.</p> </li> <li> <p> <a>UpdateAlias</a> -- Change settings for a alias, such as redirecting it from one fleet to another.</p> </li> <li> <p> <a>DeleteAlias</a> -- Remove an alias from the region.</p> </li> <li> <p> <a>ResolveAlias</a> -- Get the fleet ID that a specified alias points to.</p> </li> </ul> </li> <li> <p> <b>Manage game session queues</b> </p> <ul> <li> <p> <a>CreateGameSessionQueue</a> -- Create a queue for processing requests for new game sessions. </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> -- Retrieve game session queues defined in a Amazon GameLift region.</p> </li> <li> <p> <a>UpdateGameSessionQueue</a> -- Change the configuration of a game session queue.</p> </li> <li> <p> <a>DeleteGameSessionQueue</a> -- Remove a game session queue from the region.</p> </li> </ul> </li> <li> <p> <b>Manage FlexMatch resources</b> </p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> -- Create a matchmaking configuration with instructions for building a player group and placing in a new game session. </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> -- Retrieve matchmaking configurations defined a Amazon GameLift region.</p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> -- Change settings for matchmaking configuration. queue.</p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> -- Remove a matchmaking configuration from the region.</p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> -- Create a set of rules to use when searching for player matches. </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> -- Retrieve matchmaking rule sets defined in a Amazon GameLift region.</p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> -- Verify syntax for a set of matchmaking rules. </p> </li> </ul> </li> </ul>
module AWS.GameLift where

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

serviceName = "GameLift" :: String


-- | <p>Registers a player's acceptance or rejection of a proposed FlexMatch match. A matchmaking configuration may require player acceptance; if so, then matches built with that configuration cannot be completed unless all players accept the proposed match within a specified time limit. </p> <p>When FlexMatch builds a match, all the matchmaking tickets involved in the proposed match are placed into status <code>REQUIRES_ACCEPTANCE</code>. This is a trigger for your game to get acceptance from all players in the ticket. Acceptances are only valid for tickets when they are in this status; all other acceptances result in an error.</p> <p>To register acceptance, specify the ticket ID, a response, and one or more players. Once all players have registered acceptance, the matchmaking tickets advance to status <code>PLACING</code>, where a new game session is created for the match. </p> <p>If any player rejects the match, or if acceptances are not received before a specified timeout, the proposed match is dropped. The matchmaking tickets are then handled in one of two ways: For tickets where all players accepted the match, the ticket status is returned to <code>SEARCHING</code> to find a new match. For tickets where one or more players failed to accept the match, the ticket status is set to <code>FAILED</code>, and processing is terminated. A new matchmaking request for these players can be submitted as needed. </p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>
acceptMatch :: forall eff. AcceptMatchInput -> Aff (exception :: EXCEPTION | eff) AcceptMatchOutput
acceptMatch = Request.request serviceName "acceptMatch" 


-- | <p>Creates an alias for a fleet. In most situations, you can use an alias ID in place of a fleet ID. By using a fleet alias instead of a specific fleet ID, you can switch gameplay and players to a new fleet without changing your game client or other game components. For example, for games in production, using an alias allows you to seamlessly redirect your player base to a new game server update. </p> <p>Amazon GameLift supports two types of routing strategies for aliases: simple and terminal. A simple alias points to an active fleet. A terminal alias is used to display messaging or link to a URL instead of routing players to an active fleet. For example, you might use a terminal alias when a game version is no longer supported and you want to direct players to an upgrade site. </p> <p>To create a fleet alias, specify an alias name, routing strategy, and optional description. Each simple alias can point to only one fleet, but a fleet can have multiple aliases. If successful, a new alias record is returned, including an alias ID, which you can reference when creating a game session. You can reassign an alias to another fleet by calling <code>UpdateAlias</code>.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
createAlias :: forall eff. CreateAliasInput -> Aff (exception :: EXCEPTION | eff) CreateAliasOutput
createAlias = Request.request serviceName "createAlias" 


-- | <p>Creates a new Amazon GameLift build record for your game server binary files and points to the location of your game server build files in an Amazon Simple Storage Service (Amazon S3) location. </p> <p>Game server binaries must be combined into a <code>.zip</code> file for use with Amazon GameLift. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html">Uploading Your Game</a> for more information. </p> <important> <p>To create new builds quickly and easily, use the AWS CLI command <b> <a href="http://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html">upload-build</a> </b>. This helper command uploads your build and creates a new build record in one step, and automatically handles the necessary permissions. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html"> Upload Build Files to Amazon GameLift</a> for more help.</p> </important> <p>The <code>CreateBuild</code> operation should be used only when you need to manually upload your build files, as in the following scenarios:</p> <ul> <li> <p>Store a build file in an Amazon S3 bucket under your own AWS account. To use this option, you must first give Amazon GameLift access to that Amazon S3 bucket. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build"> Create a Build with Files in Amazon S3</a> for detailed help. To create a new build record using files in your Amazon S3 bucket, call <code>CreateBuild</code> and specify a build name, operating system, and the storage location of your game build.</p> </li> <li> <p>Upload a build file directly to Amazon GameLift's Amazon S3 account. To use this option, you first call <code>CreateBuild</code> with a build name and operating system. This action creates a new build record and returns an Amazon S3 storage location (bucket and key only) and temporary access credentials. Use the credentials to manually upload your build file to the storage location (see the Amazon S3 topic <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html">Uploading Objects</a>). You can upload files to a location only once. </p> </li> </ul> <p>If successful, this operation creates a new build record with a unique build ID and places it in <code>INITIALIZED</code> status. You can use <a>DescribeBuild</a> to check the status of your build. A build must be in <code>READY</code> status before it can be used to create fleets.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>
createBuild :: forall eff. CreateBuildInput -> Aff (exception :: EXCEPTION | eff) CreateBuildOutput
createBuild = Request.request serviceName "createBuild" 


-- | <p>Creates a new fleet to run your game servers. A fleet is a set of Amazon Elastic Compute Cloud (Amazon EC2) instances, each of which can run multiple server processes to host game sessions. You set up a fleet to use instances with certain hardware specifications (see <a href="http://aws.amazon.com/ec2/instance-types/">Amazon EC2 Instance Types</a> for more information), and deploy your game build to run on each instance. </p> <p>To create a new fleet, you must specify the following: (1) a fleet name, (2) the build ID of a successfully uploaded game build, (3) an EC2 instance type, and (4) a run-time configuration, which describes the server processes to run on each instance in the fleet. If you don't specify a fleet type (on-demand or spot), the new fleet uses on-demand instances by default.</p> <p>You can also configure the new fleet with the following settings:</p> <ul> <li> <p>Fleet description</p> </li> <li> <p>Access permissions for inbound traffic</p> </li> <li> <p>Fleet-wide game session protection</p> </li> <li> <p>Resource usage limits</p> </li> </ul> <ul> <li> <p>VPC peering connection (see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html">VPC Peering with Amazon GameLift Fleets</a>)</p> </li> </ul> <p>If you use Amazon CloudWatch for metrics, you can add the new fleet to a metric group. By adding multiple fleets to a metric group, you can view aggregated metrics for all the fleets in the group. </p> <p>If the <code>CreateFleet</code> call is successful, Amazon GameLift performs the following tasks. You can track the process of a fleet by checking the fleet status or by monitoring fleet creation events:</p> <ul> <li> <p>Creates a fleet record. Status: <code>NEW</code>.</p> </li> <li> <p>Begins writing events to the fleet event log, which can be accessed in the Amazon GameLift console.</p> <p>Sets the fleet's target capacity to 1 (desired instances), which triggers Amazon GameLift to start one new EC2 instance.</p> </li> <li> <p>Downloads the game build to the new instance and installs it. Statuses: <code>DOWNLOADING</code>, <code>VALIDATING</code>, <code>BUILDING</code>. </p> </li> <li> <p>Starts launching server processes on the instance. If the fleet is configured to run multiple server processes per instance, Amazon GameLift staggers each launch by a few seconds. Status: <code>ACTIVATING</code>.</p> </li> <li> <p>Sets the fleet's status to <code>ACTIVE</code> as soon as one server process is ready to host a game session.</p> </li> </ul> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
createFleet :: forall eff. CreateFleetInput -> Aff (exception :: EXCEPTION | eff) CreateFleetOutput
createFleet = Request.request serviceName "createFleet" 


-- | <p>Creates a multiplayer game session for players. This action creates a game session record and assigns an available server process in the specified fleet to host the game session. A fleet must have an <code>ACTIVE</code> status before a game session can be created in it.</p> <p>To create a game session, specify either fleet ID or alias ID and indicate a maximum number of players to allow in the game session. You can also provide a name and game-specific properties for this game session. If successful, a <a>GameSession</a> object is returned containing the game session properties and other settings you specified.</p> <p> <b>Idempotency tokens.</b> You can add a token that uniquely identifies game session requests. This is useful for ensuring that game session requests are idempotent. Multiple requests with the same idempotency token are processed only once; subsequent requests return the original result. All response values are the same with the exception of game session status, which may change.</p> <p> <b>Resource creation limits.</b> If you are creating a game session on a fleet with a resource creation limit policy in force, then you must specify a creator ID. Without this ID, Amazon GameLift has no way to evaluate the policy for this new game session request.</p> <p> <b>Player acceptance policy.</b> By default, newly created game sessions are open to new players. You can restrict new player access by using <a>UpdateGameSession</a> to change the game session's player session creation policy.</p> <p> <b>Game session logs.</b> Logs are retained for all active game sessions for 14 days. To access the logs, call <a>GetGameSessionLogUrl</a> to download the log files.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
createGameSession :: forall eff. CreateGameSessionInput -> Aff (exception :: EXCEPTION | eff) CreateGameSessionOutput
createGameSession = Request.request serviceName "createGameSession" 


-- | <p>Establishes a new queue for processing requests to place new game sessions. A queue identifies where new game sessions can be hosted -- by specifying a list of destinations (fleets or aliases) -- and how long requests can wait in the queue before timing out. You can set up a queue to try to place game sessions on fleets in multiple regions. To add placement requests to a queue, call <a>StartGameSessionPlacement</a> and reference the queue name.</p> <p> <b>Destination order.</b> When processing a request for a game session, Amazon GameLift tries each destination in order until it finds one with available resources to host the new game session. A queue's default order is determined by how destinations are listed. The default order is overridden when a game session placement request provides player latency information. Player latency information enables Amazon GameLift to prioritize destinations where players report the lowest average latency, as a result placing the new game session where the majority of players will have the best possible gameplay experience.</p> <p> <b>Player latency policies.</b> For placement requests containing player latency information, use player latency policies to protect individual players from very high latencies. With a latency cap, even when a destination can deliver a low latency for most players, the game is not placed where any individual player is reporting latency higher than a policy's maximum. A queue can have multiple latency policies, which are enforced consecutively starting with the policy with the lowest latency cap. Use multiple policies to gradually relax latency controls; for example, you might set a policy with a low latency cap for the first 60 seconds, a second policy with a higher cap for the next 60 seconds, etc. </p> <p>To create a new queue, provide a name, timeout value, a list of destinations and, if desired, a set of latency policies. If successful, a new queue object is returned.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
createGameSessionQueue :: forall eff. CreateGameSessionQueueInput -> Aff (exception :: EXCEPTION | eff) CreateGameSessionQueueOutput
createGameSessionQueue = Request.request serviceName "createGameSessionQueue" 


-- | <p>Defines a new matchmaking configuration for use with FlexMatch. A matchmaking configuration sets out guidelines for matching players and getting the matches into games. You can set up multiple matchmaking configurations to handle the scenarios needed for your game. Each matchmaking ticket (<a>StartMatchmaking</a> or <a>StartMatchBackfill</a>) specifies a configuration for the match and provides player attributes to support the configuration being used. </p> <p>To create a matchmaking configuration, at a minimum you must specify the following: configuration name; a rule set that governs how to evaluate players and find acceptable matches; a game session queue to use when placing a new game session for the match; and the maximum time allowed for a matchmaking attempt.</p> <p> <b>Player acceptance</b> -- In each configuration, you have the option to require that all players accept participation in a proposed match. To enable this feature, set <i>AcceptanceRequired</i> to true and specify a time limit for player acceptance. Players have the option to accept or reject a proposed match, and a match does not move ahead to game session placement unless all matched players accept. </p> <p> <b>Matchmaking status notification</b> -- There are two ways to track the progress of matchmaking tickets: (1) polling ticket status with <a>DescribeMatchmaking</a>; or (2) receiving notifications with Amazon Simple Notification Service (SNS). To use notifications, you first need to set up an SNS topic to receive the notifications, and provide the topic ARN in the matchmaking configuration (see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-notification.html"> Setting up Notifications for Matchmaking</a>). Since notifications promise only "best effort" delivery, we recommend calling <code>DescribeMatchmaking</code> if no notifications are received within 30 seconds.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
createMatchmakingConfiguration :: forall eff. CreateMatchmakingConfigurationInput -> Aff (exception :: EXCEPTION | eff) CreateMatchmakingConfigurationOutput
createMatchmakingConfiguration = Request.request serviceName "createMatchmakingConfiguration" 


-- | <p>Creates a new rule set for FlexMatch matchmaking. A rule set describes the type of match to create, such as the number and size of teams, and sets the parameters for acceptable player matches, such as minimum skill level or character type. Rule sets are used in matchmaking configurations, which define how matchmaking requests are handled. Each <a>MatchmakingConfiguration</a> uses one rule set; you can set up multiple rule sets to handle the scenarios that suit your game (such as for different game modes), and create a separate matchmaking configuration for each rule set. See additional information on rule set content in the <a>MatchmakingRuleSet</a> structure. For help creating rule sets, including useful examples, see the topic <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html"> Adding FlexMatch to Your Game</a>.</p> <p>Once created, matchmaking rule sets cannot be changed or deleted, so we recommend checking the rule set syntax using <a>ValidateMatchmakingRuleSet</a> before creating the rule set.</p> <p>To create a matchmaking rule set, provide the set of rules and a unique name. Rule sets must be defined in the same region as the matchmaking configuration they will be used with. Rule sets cannot be edited or deleted. If you need to change a rule set, create a new one with the necessary edits and then update matchmaking configurations to use the new rule set.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
createMatchmakingRuleSet :: forall eff. CreateMatchmakingRuleSetInput -> Aff (exception :: EXCEPTION | eff) CreateMatchmakingRuleSetOutput
createMatchmakingRuleSet = Request.request serviceName "createMatchmakingRuleSet" 


-- | <p>Adds a player to a game session and creates a player session record. Before a player can be added, a game session must have an <code>ACTIVE</code> status, have a creation policy of <code>ALLOW_ALL</code>, and have an open player slot. To add a group of players to a game session, use <a>CreatePlayerSessions</a>.</p> <p>To create a player session, specify a game session ID, player ID, and optionally a string of player data. If successful, the player is added to the game session and a new <a>PlayerSession</a> object is returned. Player sessions cannot be updated. </p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
createPlayerSession :: forall eff. CreatePlayerSessionInput -> Aff (exception :: EXCEPTION | eff) CreatePlayerSessionOutput
createPlayerSession = Request.request serviceName "createPlayerSession" 


-- | <p>Adds a group of players to a game session. This action is useful with a team matching feature. Before players can be added, a game session must have an <code>ACTIVE</code> status, have a creation policy of <code>ALLOW_ALL</code>, and have an open player slot. To add a single player to a game session, use <a>CreatePlayerSession</a>.</p> <p>To create player sessions, specify a game session ID, a list of player IDs, and optionally a set of player data strings. If successful, the players are added to the game session and a set of new <a>PlayerSession</a> objects is returned. Player sessions cannot be updated.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
createPlayerSessions :: forall eff. CreatePlayerSessionsInput -> Aff (exception :: EXCEPTION | eff) CreatePlayerSessionsOutput
createPlayerSessions = Request.request serviceName "createPlayerSessions" 


-- | <p>Requests authorization to create or delete a peer connection between the VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in your AWS account. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. Once you've received authorization, call <a>CreateVpcPeeringConnection</a> to establish the peering connection. For more information, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html">VPC Peering with Amazon GameLift Fleets</a>.</p> <p>You can peer with VPCs that are owned by any AWS account you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different regions.</p> <p>To request authorization to create a connection, call this operation from the AWS account with the VPC that you want to peer to your Amazon GameLift fleet. For example, to enable your game servers to retrieve data from a DynamoDB table, use the account that manages that DynamoDB resource. Identify the following values: (1) The ID of the VPC that you want to peer with, and (2) the ID of the AWS account that you use to manage Amazon GameLift. If successful, VPC peering is authorized for the specified VPC. </p> <p>To request authorization to delete a connection, call this operation from the AWS account with the VPC that is peered with your Amazon GameLift fleet. Identify the following values: (1) VPC ID that you want to delete the peering connection for, and (2) ID of the AWS account that you use to manage Amazon GameLift. </p> <p>The authorization remains valid for 24 hours unless it is canceled by a call to <a>DeleteVpcPeeringAuthorization</a>. You must create or delete the peering connection while the authorization is valid. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
createVpcPeeringAuthorization :: forall eff. CreateVpcPeeringAuthorizationInput -> Aff (exception :: EXCEPTION | eff) CreateVpcPeeringAuthorizationOutput
createVpcPeeringAuthorization = Request.request serviceName "createVpcPeeringAuthorization" 


-- | <p>Establishes a VPC peering connection between a virtual private cloud (VPC) in an AWS account with the VPC for your Amazon GameLift fleet. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. You can peer with VPCs in any AWS account that you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different regions. For more information, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html">VPC Peering with Amazon GameLift Fleets</a>.</p> <p>Before calling this operation to establish the peering connection, you first need to call <a>CreateVpcPeeringAuthorization</a> and identify the VPC you want to peer with. Once the authorization for the specified VPC is issued, you have 24 hours to establish the connection. These two operations handle all tasks necessary to peer the two VPCs, including acceptance, updating routing tables, etc. </p> <p>To establish the connection, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the following values: (1) The ID of the fleet you want to be enable a VPC peering connection for; (2) The AWS account with the VPC that you want to peer with; and (3) The ID of the VPC you want to peer with. This operation is asynchronous. If successful, a <a>VpcPeeringConnection</a> request is created. You can use continuous polling to track the request's status using <a>DescribeVpcPeeringConnections</a>, or by monitoring fleet events for success or failure using <a>DescribeFleetEvents</a>. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
createVpcPeeringConnection :: forall eff. CreateVpcPeeringConnectionInput -> Aff (exception :: EXCEPTION | eff) CreateVpcPeeringConnectionOutput
createVpcPeeringConnection = Request.request serviceName "createVpcPeeringConnection" 


-- | <p>Deletes an alias. This action removes all record of the alias. Game clients attempting to access a server process using the deleted alias receive an error. To delete an alias, specify the alias ID to be deleted.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
deleteAlias :: forall eff. DeleteAliasInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteAlias = Request.request serviceName "deleteAlias" 


-- | <p>Deletes a build. This action permanently deletes the build record and any uploaded build files.</p> <p>To delete a build, specify its ID. Deleting a build does not affect the status of any active fleets using the build, but you can no longer create new fleets with the deleted build.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>
deleteBuild :: forall eff. DeleteBuildInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBuild = Request.request serviceName "deleteBuild" 


-- | <p>Deletes everything related to a fleet. Before deleting a fleet, you must set the fleet's desired capacity to zero. See <a>UpdateFleetCapacity</a>.</p> <p>This action removes the fleet's resources and the fleet record. Once a fleet is deleted, you can no longer use that fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
deleteFleet :: forall eff. DeleteFleetInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteFleet = Request.request serviceName "deleteFleet" 


-- | <p>Deletes a game session queue. This action means that any <a>StartGameSessionPlacement</a> requests that reference this queue will fail. To delete a queue, specify the queue name.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
deleteGameSessionQueue :: forall eff. DeleteGameSessionQueueInput -> Aff (exception :: EXCEPTION | eff) DeleteGameSessionQueueOutput
deleteGameSessionQueue = Request.request serviceName "deleteGameSessionQueue" 


-- | <p>Permanently removes a FlexMatch matchmaking configuration. To delete, specify the configuration name. A matchmaking configuration cannot be deleted if it is being used in any active matchmaking tickets.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
deleteMatchmakingConfiguration :: forall eff. DeleteMatchmakingConfigurationInput -> Aff (exception :: EXCEPTION | eff) DeleteMatchmakingConfigurationOutput
deleteMatchmakingConfiguration = Request.request serviceName "deleteMatchmakingConfiguration" 


-- | <p>Deletes a fleet scaling policy. This action means that the policy is no longer in force and removes all record of it. To delete a scaling policy, specify both the scaling policy name and the fleet ID it is associated with.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
deleteScalingPolicy :: forall eff. DeleteScalingPolicyInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteScalingPolicy = Request.request serviceName "deleteScalingPolicy" 


-- | <p>Cancels a pending VPC peering authorization for the specified VPC. If the authorization has already been used to create a peering connection, call <a>DeleteVpcPeeringConnection</a> to remove the connection. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
deleteVpcPeeringAuthorization :: forall eff. DeleteVpcPeeringAuthorizationInput -> Aff (exception :: EXCEPTION | eff) DeleteVpcPeeringAuthorizationOutput
deleteVpcPeeringAuthorization = Request.request serviceName "deleteVpcPeeringAuthorization" 


-- | <p>Removes a VPC peering connection. To delete the connection, you must have a valid authorization for the VPC peering connection that you want to delete. You can check for an authorization by calling <a>DescribeVpcPeeringAuthorizations</a> or request a new one using <a>CreateVpcPeeringAuthorization</a>. </p> <p>Once a valid authorization exists, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the connection to delete by the connection ID and fleet ID. If successful, the connection is removed. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
deleteVpcPeeringConnection :: forall eff. DeleteVpcPeeringConnectionInput -> Aff (exception :: EXCEPTION | eff) DeleteVpcPeeringConnectionOutput
deleteVpcPeeringConnection = Request.request serviceName "deleteVpcPeeringConnection" 


-- | <p>Retrieves properties for an alias. This operation returns all alias metadata and settings. To get an alias's target fleet ID only, use <code>ResolveAlias</code>. </p> <p>To get alias properties, specify the alias ID. If successful, the requested alias record is returned.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
describeAlias :: forall eff. DescribeAliasInput -> Aff (exception :: EXCEPTION | eff) DescribeAliasOutput
describeAlias = Request.request serviceName "describeAlias" 


-- | <p>Retrieves properties for a build. To request a build record, specify a build ID. If successful, an object containing the build properties is returned.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>
describeBuild :: forall eff. DescribeBuildInput -> Aff (exception :: EXCEPTION | eff) DescribeBuildOutput
describeBuild = Request.request serviceName "describeBuild" 


-- | <p>Retrieves the following information for the specified EC2 instance type:</p> <ul> <li> <p>maximum number of instances allowed per AWS account (service limit)</p> </li> <li> <p>current usage level for the AWS account</p> </li> </ul> <p>Service limits vary depending on region. Available regions for Amazon GameLift can be found in the AWS Management Console for Amazon GameLift (see the drop-down list in the upper right corner).</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeEC2InstanceLimits :: forall eff. DescribeEC2InstanceLimitsInput -> Aff (exception :: EXCEPTION | eff) DescribeEC2InstanceLimitsOutput
describeEC2InstanceLimits = Request.request serviceName "describeEC2InstanceLimits" 


-- | <p>Retrieves fleet properties, including metadata, status, and configuration, for one or more fleets. You can request attributes for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>FleetAttributes</a> object is returned for each requested fleet ID. When specifying a list of fleet IDs, attribute objects are returned only for fleets that currently exist. </p> <note> <p>Some API actions may limit the number of fleet IDs allowed in one request. If a request exceeds this limit, the request fails and the error message includes the maximum allowed.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeFleetAttributes :: forall eff. DescribeFleetAttributesInput -> Aff (exception :: EXCEPTION | eff) DescribeFleetAttributesOutput
describeFleetAttributes = Request.request serviceName "describeFleetAttributes" 


-- | <p>Retrieves the current status of fleet capacity for one or more fleets. This information includes the number of instances that have been requested for the fleet and the number currently active. You can request capacity for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>FleetCapacity</a> object is returned for each requested fleet ID. When specifying a list of fleet IDs, attribute objects are returned only for fleets that currently exist. </p> <note> <p>Some API actions may limit the number of fleet IDs allowed in one request. If a request exceeds this limit, the request fails and the error message includes the maximum allowed.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeFleetCapacity :: forall eff. DescribeFleetCapacityInput -> Aff (exception :: EXCEPTION | eff) DescribeFleetCapacityOutput
describeFleetCapacity = Request.request serviceName "describeFleetCapacity" 


-- | <p>Retrieves entries from the specified fleet's event log. You can specify a time range to limit the result set. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a collection of event log entries matching the request are returned.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeFleetEvents :: forall eff. DescribeFleetEventsInput -> Aff (exception :: EXCEPTION | eff) DescribeFleetEventsOutput
describeFleetEvents = Request.request serviceName "describeFleetEvents" 


-- | <p>Retrieves the inbound connection permissions for a fleet. Connection permissions include a range of IP addresses and port settings that incoming traffic can use to access server processes in the fleet. To get a fleet's inbound connection permissions, specify a fleet ID. If successful, a collection of <a>IpPermission</a> objects is returned for the requested fleet ID. If the requested fleet has been deleted, the result set is empty.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeFleetPortSettings :: forall eff. DescribeFleetPortSettingsInput -> Aff (exception :: EXCEPTION | eff) DescribeFleetPortSettingsOutput
describeFleetPortSettings = Request.request serviceName "describeFleetPortSettings" 


-- | <p>Retrieves utilization statistics for one or more fleets. You can request utilization data for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>FleetUtilization</a> object is returned for each requested fleet ID. When specifying a list of fleet IDs, utilization objects are returned only for fleets that currently exist. </p> <note> <p>Some API actions may limit the number of fleet IDs allowed in one request. If a request exceeds this limit, the request fails and the error message includes the maximum allowed.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeFleetUtilization :: forall eff. DescribeFleetUtilizationInput -> Aff (exception :: EXCEPTION | eff) DescribeFleetUtilizationOutput
describeFleetUtilization = Request.request serviceName "describeFleetUtilization" 


-- | <p>Retrieves properties, including the protection policy in force, for one or more game sessions. This action can be used in several ways: (1) provide a <code>GameSessionId</code> or <code>GameSessionArn</code> to request details for a specific game session; (2) provide either a <code>FleetId</code> or an <code>AliasId</code> to request properties for all game sessions running on a fleet. </p> <p>To get game session record(s), specify just one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>GameSessionDetail</a> object is returned for each session matching the request.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
describeGameSessionDetails :: forall eff. DescribeGameSessionDetailsInput -> Aff (exception :: EXCEPTION | eff) DescribeGameSessionDetailsOutput
describeGameSessionDetails = Request.request serviceName "describeGameSessionDetails" 


-- | <p>Retrieves properties and current status of a game session placement request. To get game session placement details, specify the placement ID. If successful, a <a>GameSessionPlacement</a> object is returned.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
describeGameSessionPlacement :: forall eff. DescribeGameSessionPlacementInput -> Aff (exception :: EXCEPTION | eff) DescribeGameSessionPlacementOutput
describeGameSessionPlacement = Request.request serviceName "describeGameSessionPlacement" 


-- | <p>Retrieves the properties for one or more game session queues. When requesting multiple queues, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>GameSessionQueue</a> object is returned for each requested queue. When specifying a list of queues, objects are returned only for queues that currently exist in the region.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
describeGameSessionQueues :: forall eff. DescribeGameSessionQueuesInput -> Aff (exception :: EXCEPTION | eff) DescribeGameSessionQueuesOutput
describeGameSessionQueues = Request.request serviceName "describeGameSessionQueues" 


-- | <p>Retrieves a set of one or more game sessions. Request a specific game session or request all game sessions on a fleet. Alternatively, use <a>SearchGameSessions</a> to request a set of active game sessions that are filtered by certain criteria. To retrieve protection policy settings for game sessions, use <a>DescribeGameSessionDetails</a>.</p> <p>To get game sessions, specify one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>GameSession</a> object is returned for each game session matching the request.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
describeGameSessions :: forall eff. DescribeGameSessionsInput -> Aff (exception :: EXCEPTION | eff) DescribeGameSessionsOutput
describeGameSessions = Request.request serviceName "describeGameSessions" 


-- | <p>Retrieves information about a fleet's instances, including instance IDs. Use this action to get details on all instances in the fleet or get details on one specific instance.</p> <p>To get a specific instance, specify fleet ID and instance ID. To get all instances in a fleet, specify a fleet ID only. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, an <a>Instance</a> object is returned for each result.</p>
describeInstances :: forall eff. DescribeInstancesInput -> Aff (exception :: EXCEPTION | eff) DescribeInstancesOutput
describeInstances = Request.request serviceName "describeInstances" 


-- | <p>Retrieves one or more matchmaking tickets. Use this operation to retrieve ticket information, including status and--once a successful match is made--acquire connection information for the resulting new game session. </p> <p>You can use this operation to track the progress of matchmaking requests (through polling) as an alternative to using event notifications. See more details on tracking matchmaking requests through polling or notifications in <a>StartMatchmaking</a>. </p> <p>To request matchmaking tickets, provide a list of up to 10 ticket IDs. If the request is successful, a ticket object is returned for each requested ID that currently exists.</p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>
describeMatchmaking :: forall eff. DescribeMatchmakingInput -> Aff (exception :: EXCEPTION | eff) DescribeMatchmakingOutput
describeMatchmaking = Request.request serviceName "describeMatchmaking" 


-- | <p>Retrieves the details of FlexMatch matchmaking configurations. with this operation, you have the following options: (1) retrieve all existing configurations, (2) provide the names of one or more configurations to retrieve, or (3) retrieve all configurations that use a specified rule set name. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a configuration is returned for each requested name. When specifying a list of names, only configurations that currently exist are returned. </p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
describeMatchmakingConfigurations :: forall eff. DescribeMatchmakingConfigurationsInput -> Aff (exception :: EXCEPTION | eff) DescribeMatchmakingConfigurationsOutput
describeMatchmakingConfigurations = Request.request serviceName "describeMatchmakingConfigurations" 


-- | <p>Retrieves the details for FlexMatch matchmaking rule sets. You can request all existing rule sets for the region, or provide a list of one or more rule set names. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a rule set is returned for each requested name. </p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
describeMatchmakingRuleSets :: forall eff. DescribeMatchmakingRuleSetsInput -> Aff (exception :: EXCEPTION | eff) DescribeMatchmakingRuleSetsOutput
describeMatchmakingRuleSets = Request.request serviceName "describeMatchmakingRuleSets" 


-- | <p>Retrieves properties for one or more player sessions. This action can be used in several ways: (1) provide a <code>PlayerSessionId</code> to request properties for a specific player session; (2) provide a <code>GameSessionId</code> to request properties for all player sessions in the specified game session; (3) provide a <code>PlayerId</code> to request properties for all player sessions of a specified player. </p> <p>To get game session record(s), specify only one of the following: a player session ID, a game session ID, or a player ID. You can filter this request by player session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>PlayerSession</a> object is returned for each session matching the request.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
describePlayerSessions :: forall eff. DescribePlayerSessionsInput -> Aff (exception :: EXCEPTION | eff) DescribePlayerSessionsOutput
describePlayerSessions = Request.request serviceName "describePlayerSessions" 


-- | <p>Retrieves the current run-time configuration for the specified fleet. The run-time configuration tells Amazon GameLift how to launch server processes on instances in the fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeRuntimeConfiguration :: forall eff. DescribeRuntimeConfigurationInput -> Aff (exception :: EXCEPTION | eff) DescribeRuntimeConfigurationOutput
describeRuntimeConfiguration = Request.request serviceName "describeRuntimeConfiguration" 


-- | <p>Retrieves all scaling policies applied to a fleet.</p> <p>To get a fleet's scaling policies, specify the fleet ID. You can filter this request by policy status, such as to retrieve only active scaling policies. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, set of <a>ScalingPolicy</a> objects is returned for the fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
describeScalingPolicies :: forall eff. DescribeScalingPoliciesInput -> Aff (exception :: EXCEPTION | eff) DescribeScalingPoliciesOutput
describeScalingPolicies = Request.request serviceName "describeScalingPolicies" 


-- | <p>Retrieves valid VPC peering authorizations that are pending for the AWS account. This operation returns all VPC peering authorizations and requests for peering. This includes those initiated and received by this account. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
describeVpcPeeringAuthorizations :: forall eff. DescribeVpcPeeringAuthorizationsInput -> Aff (exception :: EXCEPTION | eff) DescribeVpcPeeringAuthorizationsOutput
describeVpcPeeringAuthorizations = Request.request serviceName "describeVpcPeeringAuthorizations" 


-- | <p>Retrieves information on VPC peering connections. Use this operation to get peering information for all fleets or for one specific fleet ID. </p> <p>To retrieve connection information, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Specify a fleet ID or leave the parameter empty to retrieve all connection records. If successful, the retrieved information includes both active and pending connections. Active connections identify the IpV4 CIDR block that the VPC uses to connect. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
describeVpcPeeringConnections :: forall eff. DescribeVpcPeeringConnectionsInput -> Aff (exception :: EXCEPTION | eff) DescribeVpcPeeringConnectionsOutput
describeVpcPeeringConnections = Request.request serviceName "describeVpcPeeringConnections" 


-- | <p>Retrieves the location of stored game session logs for a specified game session. When a game session is terminated, Amazon GameLift automatically stores the logs in Amazon S3 and retains them for 14 days. Use this URL to download the logs.</p> <note> <p>See the <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_gamelift">AWS Service Limits</a> page for maximum log file sizes. Log files that exceed this limit are not saved.</p> </note> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
getGameSessionLogUrl :: forall eff. GetGameSessionLogUrlInput -> Aff (exception :: EXCEPTION | eff) GetGameSessionLogUrlOutput
getGameSessionLogUrl = Request.request serviceName "getGameSessionLogUrl" 


-- | <p>Requests remote access to a fleet instance. Remote access is useful for debugging, gathering benchmarking data, or watching activity in real time. </p> <p>Access requires credentials that match the operating system of the instance. For a Windows instance, Amazon GameLift returns a user name and password as strings for use with a Windows Remote Desktop client. For a Linux instance, Amazon GameLift returns a user name and RSA private key, also as strings, for use with an SSH client. The private key must be saved in the proper format to a <code>.pem</code> file before using. If you're making this request using the AWS CLI, saving the secret can be handled as part of the GetInstanceAccess request. (See the example later in this topic). For more information on remote access, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html">Remotely Accessing an Instance</a>.</p> <p>To request access to a specific instance, specify the IDs of the instance and the fleet it belongs to. If successful, an <a>InstanceAccess</a> object is returned containing the instance's IP address and a set of credentials.</p>
getInstanceAccess :: forall eff. GetInstanceAccessInput -> Aff (exception :: EXCEPTION | eff) GetInstanceAccessOutput
getInstanceAccess = Request.request serviceName "getInstanceAccess" 


-- | <p>Retrieves all aliases for this AWS account. You can filter the result set by alias name and/or routing strategy type. Use the pagination parameters to retrieve results in sequential pages.</p> <note> <p>Returned aliases are not listed in any particular order.</p> </note> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
listAliases :: forall eff. ListAliasesInput -> Aff (exception :: EXCEPTION | eff) ListAliasesOutput
listAliases = Request.request serviceName "listAliases" 


-- | <p>Retrieves build records for all builds associated with the AWS account in use. You can limit results to builds that are in a specific status by using the <code>Status</code> parameter. Use the pagination parameters to retrieve results in a set of sequential pages. </p> <note> <p>Build records are not listed in any particular order.</p> </note> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>
listBuilds :: forall eff. ListBuildsInput -> Aff (exception :: EXCEPTION | eff) ListBuildsOutput
listBuilds = Request.request serviceName "listBuilds" 


-- | <p>Retrieves a collection of fleet records for this AWS account. You can filter the result set by build ID. Use the pagination parameters to retrieve results in sequential pages.</p> <note> <p>Fleet records are not listed in any particular order.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
listFleets :: forall eff. ListFleetsInput -> Aff (exception :: EXCEPTION | eff) ListFleetsOutput
listFleets = Request.request serviceName "listFleets" 


-- | <p>Creates or updates a scaling policy for a fleet. An active scaling policy prompts Amazon GameLift to track a certain metric for a fleet and automatically change the fleet's capacity in specific circumstances. Each scaling policy contains one rule statement. Fleets can have multiple scaling policies in force simultaneously.</p> <p>A scaling policy rule statement has the following structure:</p> <p>If <code>[MetricName]</code> is <code>[ComparisonOperator]</code> <code>[Threshold]</code> for <code>[EvaluationPeriods]</code> minutes, then <code>[ScalingAdjustmentType]</code> to/by <code>[ScalingAdjustment]</code>.</p> <p>For example, this policy: "If the number of idle instances exceeds 20 for more than 15 minutes, then reduce the fleet capacity by 10 instances" could be implemented as the following rule statement:</p> <p>If [IdleInstances] is [GreaterThanOrEqualToThreshold] [20] for [15] minutes, then [ChangeInCapacity] by [-10].</p> <p>To create or update a scaling policy, specify a unique combination of name and fleet ID, and set the rule values. All parameters for this action are required. If successful, the policy name is returned. Scaling policies cannot be suspended or made inactive. To stop enforcing a scaling policy, call <a>DeleteScalingPolicy</a>.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
putScalingPolicy :: forall eff. PutScalingPolicyInput -> Aff (exception :: EXCEPTION | eff) PutScalingPolicyOutput
putScalingPolicy = Request.request serviceName "putScalingPolicy" 


-- | <p>Retrieves a fresh set of credentials for use when uploading a new set of game build files to Amazon GameLift's Amazon S3. This is done as part of the build creation process; see <a>CreateBuild</a>.</p> <p>To request new credentials, specify the build ID as returned with an initial <code>CreateBuild</code> request. If successful, a new set of credentials are returned, along with the S3 storage location associated with the build ID.</p>
requestUploadCredentials :: forall eff. RequestUploadCredentialsInput -> Aff (exception :: EXCEPTION | eff) RequestUploadCredentialsOutput
requestUploadCredentials = Request.request serviceName "requestUploadCredentials" 


-- | <p>Retrieves the fleet ID that a specified alias is currently pointing to.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
resolveAlias :: forall eff. ResolveAliasInput -> Aff (exception :: EXCEPTION | eff) ResolveAliasOutput
resolveAlias = Request.request serviceName "resolveAlias" 


-- | <p>Retrieves all active game sessions that match a set of search criteria and sorts them in a specified order. You can search or sort by the following game session attributes:</p> <ul> <li> <p> <b>gameSessionId</b> -- Unique identifier for the game session. You can use either a <code>GameSessionId</code> or <code>GameSessionArn</code> value. </p> </li> <li> <p> <b>gameSessionName</b> -- Name assigned to a game session. This value is set when requesting a new game session with <a>CreateGameSession</a> or updating with <a>UpdateGameSession</a>. Game session names do not need to be unique to a game session.</p> </li> <li> <p> <b>gameSessionProperties</b> -- Custom data defined in a game session's <code>GameProperty</code> parameter. <code>GameProperty</code> values are stored as key:value pairs; the filter expression must indicate the key and a string to search the data values for. For example, to search for game sessions with custom data containing the key:value pair "gameMode:brawl", specify the following: gameSessionProperties.gameMode = "brawl". All custom data values are searched as strings.</p> </li> <li> <p> <b>maximumSessions</b> -- Maximum number of player sessions allowed for a game session. This value is set when requesting a new game session with <a>CreateGameSession</a> or updating with <a>UpdateGameSession</a>.</p> </li> <li> <p> <b>creationTimeMillis</b> -- Value indicating when a game session was created. It is expressed in Unix time as milliseconds.</p> </li> <li> <p> <b>playerSessionCount</b> -- Number of players currently connected to a game session. This value changes rapidly as players join the session or drop out.</p> </li> <li> <p> <b>hasAvailablePlayerSessions</b> -- Boolean value indicating whether a game session has reached its maximum number of players. It is highly recommended that all search requests include this filter attribute to optimize search performance and return only sessions that players can join. </p> </li> </ul> <note> <p>Returned values for <code>playerSessionCount</code> and <code>hasAvailablePlayerSessions</code> change quickly as players join sessions and others drop out. Results should be considered a snapshot in time. Be sure to refresh search results often, and handle sessions that fill up before a player can join. </p> </note> <p>To search or sort, specify either a fleet ID or an alias ID, and provide a search filter expression, a sort expression, or both. If successful, a collection of <a>GameSession</a> objects matching the request is returned. Use the pagination parameters to retrieve results as a set of sequential pages. </p> <p>You can search for game sessions one fleet at a time only. To find game sessions across multiple fleets, you must search each fleet separately and combine the results. This search feature finds only game sessions that are in <code>ACTIVE</code> status. To locate games in statuses other than active, use <a>DescribeGameSessionDetails</a>.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
searchGameSessions :: forall eff. SearchGameSessionsInput -> Aff (exception :: EXCEPTION | eff) SearchGameSessionsOutput
searchGameSessions = Request.request serviceName "searchGameSessions" 


-- | <p>Places a request for a new game session in a queue (see <a>CreateGameSessionQueue</a>). When processing a placement request, Amazon GameLift searches for available resources on the queue's destinations, scanning each until it finds resources or the placement request times out.</p> <p>A game session placement request can also request player sessions. When a new game session is successfully created, Amazon GameLift creates a player session for each player included in the request.</p> <p>When placing a game session, by default Amazon GameLift tries each fleet in the order they are listed in the queue configuration. Ideally, a queue's destinations are listed in preference order.</p> <p>Alternatively, when requesting a game session with players, you can also provide latency data for each player in relevant regions. Latency data indicates the performance lag a player experiences when connected to a fleet in the region. Amazon GameLift uses latency data to reorder the list of destinations to place the game session in a region with minimal lag. If latency data is provided for multiple players, Amazon GameLift calculates each region's average lag for all players and reorders to get the best game play across all players. </p> <p>To place a new game session request, specify the following:</p> <ul> <li> <p>The queue name and a set of game session properties and settings</p> </li> <li> <p>A unique ID (such as a UUID) for the placement. You use this ID to track the status of the placement request</p> </li> <li> <p>(Optional) A set of IDs and player data for each player you want to join to the new game session</p> </li> <li> <p>Latency data for all players (if you want to optimize game play for the players)</p> </li> </ul> <p>If successful, a new game session placement is created.</p> <p>To track the status of a placement request, call <a>DescribeGameSessionPlacement</a> and check the request's status. If the status is <code>FULFILLED</code>, a new game session has been created and a game session ARN and region are referenced. If the placement request times out, you can resubmit the request or retry it with a different queue. </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
startGameSessionPlacement :: forall eff. StartGameSessionPlacementInput -> Aff (exception :: EXCEPTION | eff) StartGameSessionPlacementOutput
startGameSessionPlacement = Request.request serviceName "startGameSessionPlacement" 


-- | <p>Finds new players to fill open slots in an existing game session. This operation can be used to add players to matched games that start with fewer than the maximum number of players or to replace players when they drop out. By backfilling with the same matchmaker used to create the original match, you ensure that new players meet the match criteria and maintain a consistent experience throughout the game session. You can backfill a match anytime after a game session has been created. </p> <p>To request a match backfill, specify a unique ticket ID, the existing game session's ARN, a matchmaking configuration, and a set of data that describes all current players in the game session. If successful, a match backfill ticket is created and returned with status set to QUEUED. The ticket is placed in the matchmaker's ticket pool and processed. Track the status of the ticket to respond as needed. For more detail how to set up backfilling, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-backfill.html"> Backfill Existing Games with FlexMatch</a>. </p> <p>The process of finding backfill matches is essentially identical to the initial matchmaking process. The matchmaker searches the pool and groups tickets together to form potential matches, allowing only one backfill ticket per potential match. Once the a match is formed, the matchmaker creates player sessions for the new players. All tickets in the match are updated with the game session's connection information, and the <a>GameSession</a> object is updated to include matchmaker data on the new players. For more detail on how match backfill requests are processed, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html"> How Amazon GameLift FlexMatch Works</a>. </p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>
startMatchBackfill :: forall eff. StartMatchBackfillInput -> Aff (exception :: EXCEPTION | eff) StartMatchBackfillOutput
startMatchBackfill = Request.request serviceName "startMatchBackfill" 


-- | <p>Uses FlexMatch to create a game match for a group of players based on custom matchmaking rules, and starts a new game for the matched players. Each matchmaking request specifies the type of match to build (team configuration, rules for an acceptable match, etc.). The request also specifies the players to find a match for and where to host the new game session for optimal performance. A matchmaking request might start with a single player or a group of players who want to play together. FlexMatch finds additional players as needed to fill the match. Match type, rules, and the queue used to place a new game session are defined in a <code>MatchmakingConfiguration</code>. For complete information on setting up and using FlexMatch, see the topic <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html"> Adding FlexMatch to Your Game</a>.</p> <p>To start matchmaking, provide a unique ticket ID, specify a matchmaking configuration, and include the players to be matched. You must also include a set of player attributes relevant for the matchmaking configuration. If successful, a matchmaking ticket is returned with status set to <code>QUEUED</code>. Track the status of the ticket to respond as needed and acquire game session connection information for successfully completed matches.</p> <p> <b>Tracking ticket status</b> -- A couple of options are available for tracking the status of matchmaking requests: </p> <ul> <li> <p>Polling -- Call <code>DescribeMatchmaking</code>. This operation returns the full ticket object, including current status and (for completed tickets) game session connection info. We recommend polling no more than once every 10 seconds.</p> </li> <li> <p>Notifications -- Get event notifications for changes in ticket status using Amazon Simple Notification Service (SNS). Notifications are easy to set up (see <a>CreateMatchmakingConfiguration</a>) and typically deliver match status changes faster and more efficiently than polling. We recommend that you use polling to back up to notifications (since delivery is not guaranteed) and call <code>DescribeMatchmaking</code> only when notifications are not received within 30 seconds.</p> </li> </ul> <p> <b>Processing a matchmaking request</b> -- FlexMatch handles a matchmaking request as follows: </p> <ol> <li> <p>Your client code submits a <code>StartMatchmaking</code> request for one or more players and tracks the status of the request ticket. </p> </li> <li> <p>FlexMatch uses this ticket and others in process to build an acceptable match. When a potential match is identified, all tickets in the proposed match are advanced to the next status. </p> </li> <li> <p>If the match requires player acceptance (set in the matchmaking configuration), the tickets move into status <code>REQUIRES_ACCEPTANCE</code>. This status triggers your client code to solicit acceptance from all players in every ticket involved in the match, and then call <a>AcceptMatch</a> for each player. If any player rejects or fails to accept the match before a specified timeout, the proposed match is dropped (see <code>AcceptMatch</code> for more details).</p> </li> <li> <p>Once a match is proposed and accepted, the matchmaking tickets move into status <code>PLACING</code>. FlexMatch locates resources for a new game session using the game session queue (set in the matchmaking configuration) and creates the game session based on the match data. </p> </li> <li> <p>When the match is successfully placed, the matchmaking tickets move into <code>COMPLETED</code> status. Connection information (including game session endpoint and player session) is added to the matchmaking tickets. Matched players can use the connection information to join the game. </p> </li> </ol> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>
startMatchmaking :: forall eff. StartMatchmakingInput -> Aff (exception :: EXCEPTION | eff) StartMatchmakingOutput
startMatchmaking = Request.request serviceName "startMatchmaking" 


-- | <p>Cancels a game session placement that is in <code>PENDING</code> status. To stop a placement, provide the placement ID values. If successful, the placement is moved to <code>CANCELLED</code> status.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
stopGameSessionPlacement :: forall eff. StopGameSessionPlacementInput -> Aff (exception :: EXCEPTION | eff) StopGameSessionPlacementOutput
stopGameSessionPlacement = Request.request serviceName "stopGameSessionPlacement" 


-- | <p>Cancels a matchmaking ticket that is currently being processed. To stop the matchmaking operation, specify the ticket ID. If successful, work on the ticket is stopped, and the ticket status is changed to <code>CANCELLED</code>.</p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>
stopMatchmaking :: forall eff. StopMatchmakingInput -> Aff (exception :: EXCEPTION | eff) StopMatchmakingOutput
stopMatchmaking = Request.request serviceName "stopMatchmaking" 


-- | <p>Updates properties for an alias. To update properties, specify the alias ID to be updated and provide the information to be changed. To reassign an alias to another fleet, provide an updated routing strategy. If successful, the updated alias record is returned.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
updateAlias :: forall eff. UpdateAliasInput -> Aff (exception :: EXCEPTION | eff) UpdateAliasOutput
updateAlias = Request.request serviceName "updateAlias" 


-- | <p>Updates metadata in a build record, including the build name and version. To update the metadata, specify the build ID to update and provide the new values. If successful, a build object containing the updated metadata is returned.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>
updateBuild :: forall eff. UpdateBuildInput -> Aff (exception :: EXCEPTION | eff) UpdateBuildOutput
updateBuild = Request.request serviceName "updateBuild" 


-- | <p>Updates fleet properties, including name and description, for a fleet. To update metadata, specify the fleet ID and the property values that you want to change. If successful, the fleet ID for the updated fleet is returned.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
updateFleetAttributes :: forall eff. UpdateFleetAttributesInput -> Aff (exception :: EXCEPTION | eff) UpdateFleetAttributesOutput
updateFleetAttributes = Request.request serviceName "updateFleetAttributes" 


-- | <p>Updates capacity settings for a fleet. Use this action to specify the number of EC2 instances (hosts) that you want this fleet to contain. Before calling this action, you may want to call <a>DescribeEC2InstanceLimits</a> to get the maximum capacity based on the fleet's EC2 instance type.</p> <p>If you're using autoscaling (see <a>PutScalingPolicy</a>), you may want to specify a minimum and/or maximum capacity. If you don't provide these, autoscaling can set capacity anywhere between zero and the <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_gamelift">service limits</a>.</p> <p>To update fleet capacity, specify the fleet ID and the number of instances you want the fleet to host. If successful, Amazon GameLift starts or terminates instances so that the fleet's active instance count matches the desired instance count. You can view a fleet's current capacity information by calling <a>DescribeFleetCapacity</a>. If the desired instance count is higher than the instance type's limit, the "Limit Exceeded" exception occurs.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
updateFleetCapacity :: forall eff. UpdateFleetCapacityInput -> Aff (exception :: EXCEPTION | eff) UpdateFleetCapacityOutput
updateFleetCapacity = Request.request serviceName "updateFleetCapacity" 


-- | <p>Updates port settings for a fleet. To update settings, specify the fleet ID to be updated and list the permissions you want to update. List the permissions you want to add in <code>InboundPermissionAuthorizations</code>, and permissions you want to remove in <code>InboundPermissionRevocations</code>. Permissions to be removed must match existing fleet permissions. If successful, the fleet ID for the updated fleet is returned.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
updateFleetPortSettings :: forall eff. UpdateFleetPortSettingsInput -> Aff (exception :: EXCEPTION | eff) UpdateFleetPortSettingsOutput
updateFleetPortSettings = Request.request serviceName "updateFleetPortSettings" 


-- | <p>Updates game session properties. This includes the session name, maximum player count, protection policy, which controls whether or not an active game session can be terminated during a scale-down event, and the player session creation policy, which controls whether or not new players can join the session. To update a game session, specify the game session ID and the values you want to change. If successful, an updated <a>GameSession</a> object is returned. </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
updateGameSession :: forall eff. UpdateGameSessionInput -> Aff (exception :: EXCEPTION | eff) UpdateGameSessionOutput
updateGameSession = Request.request serviceName "updateGameSession" 


-- | <p>Updates settings for a game session queue, which determines how new game session requests in the queue are processed. To update settings, specify the queue name to be updated and provide the new settings. When updating destinations, provide a complete list of destinations. </p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
updateGameSessionQueue :: forall eff. UpdateGameSessionQueueInput -> Aff (exception :: EXCEPTION | eff) UpdateGameSessionQueueOutput
updateGameSessionQueue = Request.request serviceName "updateGameSessionQueue" 


-- | <p>Updates settings for a FlexMatch matchmaking configuration. To update settings, specify the configuration name to be updated and provide the new settings. </p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
updateMatchmakingConfiguration :: forall eff. UpdateMatchmakingConfigurationInput -> Aff (exception :: EXCEPTION | eff) UpdateMatchmakingConfigurationOutput
updateMatchmakingConfiguration = Request.request serviceName "updateMatchmakingConfiguration" 


-- | <p>Updates the current run-time configuration for the specified fleet, which tells Amazon GameLift how to launch server processes on instances in the fleet. You can update a fleet's run-time configuration at any time after the fleet is created; it does not need to be in an <code>ACTIVE</code> status.</p> <p>To update run-time configuration, specify the fleet ID and provide a <code>RuntimeConfiguration</code> object with the updated collection of server process configurations.</p> <p>Each instance in a Amazon GameLift fleet checks regularly for an updated run-time configuration and changes how it launches server processes to comply with the latest version. Existing server processes are not affected by the update; they continue to run until they end, while Amazon GameLift simply adds new server processes to fit the current run-time configuration. As a result, the run-time configuration changes are applied gradually as existing processes shut down and new processes are launched in Amazon GameLift's normal process recycling activity.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
updateRuntimeConfiguration :: forall eff. UpdateRuntimeConfigurationInput -> Aff (exception :: EXCEPTION | eff) UpdateRuntimeConfigurationOutput
updateRuntimeConfiguration = Request.request serviceName "updateRuntimeConfiguration" 


-- | <p>Validates the syntax of a matchmaking rule or rule set. This operation checks that the rule set uses syntactically correct JSON and that it conforms to allowed property expressions. To validate syntax, provide a rule set string.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>
validateMatchmakingRuleSet :: forall eff. ValidateMatchmakingRuleSetInput -> Aff (exception :: EXCEPTION | eff) ValidateMatchmakingRuleSetOutput
validateMatchmakingRuleSet = Request.request serviceName "validateMatchmakingRuleSet" 


-- | <p>Represents the input for a request action.</p>
newtype AcceptMatchInput = AcceptMatchInput 
  { "TicketId" :: (MatchmakingIdStringModel)
  , "PlayerIds" :: (StringList)
  , "AcceptanceType" :: (AcceptanceType)
  }
derive instance newtypeAcceptMatchInput :: Newtype AcceptMatchInput _
derive instance repGenericAcceptMatchInput :: Generic AcceptMatchInput _
instance showAcceptMatchInput :: Show AcceptMatchInput where
  show = genericShow
instance decodeAcceptMatchInput :: Decode AcceptMatchInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptMatchInput :: Encode AcceptMatchInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AcceptMatchOutput = AcceptMatchOutput Types.NoArguments
derive instance newtypeAcceptMatchOutput :: Newtype AcceptMatchOutput _
derive instance repGenericAcceptMatchOutput :: Generic AcceptMatchOutput _
instance showAcceptMatchOutput :: Show AcceptMatchOutput where
  show = genericShow
instance decodeAcceptMatchOutput :: Decode AcceptMatchOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptMatchOutput :: Encode AcceptMatchOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AcceptanceType = AcceptanceType String
derive instance newtypeAcceptanceType :: Newtype AcceptanceType _
derive instance repGenericAcceptanceType :: Generic AcceptanceType _
instance showAcceptanceType :: Show AcceptanceType where
  show = genericShow
instance decodeAcceptanceType :: Decode AcceptanceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptanceType :: Encode AcceptanceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Properties describing a fleet alias.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>
newtype Alias = Alias 
  { "AliasId" :: NullOrUndefined.NullOrUndefined (AliasId)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonBlankAndLengthConstraintString)
  , "AliasArn" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "Description" :: NullOrUndefined.NullOrUndefined (FreeText)
  , "RoutingStrategy" :: NullOrUndefined.NullOrUndefined (RoutingStrategy)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdatedTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeAlias :: Newtype Alias _
derive instance repGenericAlias :: Generic Alias _
instance showAlias :: Show Alias where
  show = genericShow
instance decodeAlias :: Decode Alias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlias :: Encode Alias where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasId = AliasId String
derive instance newtypeAliasId :: Newtype AliasId _
derive instance repGenericAliasId :: Generic AliasId _
instance showAliasId :: Show AliasId where
  show = genericShow
instance decodeAliasId :: Decode AliasId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasId :: Encode AliasId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasList = AliasList (Array Alias)
derive instance newtypeAliasList :: Newtype AliasList _
derive instance repGenericAliasList :: Generic AliasList _
instance showAliasList :: Show AliasList where
  show = genericShow
instance decodeAliasList :: Decode AliasList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasList :: Encode AliasList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArnStringModel = ArnStringModel String
derive instance newtypeArnStringModel :: Newtype ArnStringModel _
derive instance repGenericArnStringModel :: Generic ArnStringModel _
instance showArnStringModel :: Show ArnStringModel where
  show = genericShow
instance decodeArnStringModel :: Decode ArnStringModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArnStringModel :: Encode ArnStringModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Values for use in <a>Player</a> attribute key:value pairs. This object lets you specify an attribute value using any of the valid data types: string, number, string array or data map. Each <code>AttributeValue</code> object can use only one of the available properties.</p>
newtype AttributeValue = AttributeValue 
  { "S" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "N" :: NullOrUndefined.NullOrUndefined (DoubleObject)
  , "SL" :: NullOrUndefined.NullOrUndefined (StringList)
  , "SDM" :: NullOrUndefined.NullOrUndefined (StringDoubleMap)
  }
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Temporary access credentials used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling <a>RequestUploadCredentials</a>.</p>
newtype AwsCredentials = AwsCredentials 
  { "AccessKeyId" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SecretAccessKey" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "SessionToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeAwsCredentials :: Newtype AwsCredentials _
derive instance repGenericAwsCredentials :: Generic AwsCredentials _
instance showAwsCredentials :: Show AwsCredentials where
  show = genericShow
instance decodeAwsCredentials :: Decode AwsCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsCredentials :: Encode AwsCredentials where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanModel = BooleanModel Boolean
derive instance newtypeBooleanModel :: Newtype BooleanModel _
derive instance repGenericBooleanModel :: Generic BooleanModel _
instance showBooleanModel :: Show BooleanModel where
  show = genericShow
instance decodeBooleanModel :: Decode BooleanModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanModel :: Encode BooleanModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Properties describing a game build.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>
newtype Build = Build 
  { "BuildId" :: NullOrUndefined.NullOrUndefined (BuildId)
  , "Name" :: NullOrUndefined.NullOrUndefined (FreeText)
  , "Version" :: NullOrUndefined.NullOrUndefined (FreeText)
  , "Status" :: NullOrUndefined.NullOrUndefined (BuildStatus)
  , "SizeOnDisk" :: NullOrUndefined.NullOrUndefined (PositiveLong)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeBuild :: Newtype Build _
derive instance repGenericBuild :: Generic Build _
instance showBuild :: Show Build where
  show = genericShow
instance decodeBuild :: Decode Build where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuild :: Encode Build where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildId = BuildId String
derive instance newtypeBuildId :: Newtype BuildId _
derive instance repGenericBuildId :: Generic BuildId _
instance showBuildId :: Show BuildId where
  show = genericShow
instance decodeBuildId :: Decode BuildId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildId :: Encode BuildId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildList = BuildList (Array Build)
derive instance newtypeBuildList :: Newtype BuildList _
derive instance repGenericBuildList :: Generic BuildList _
instance showBuildList :: Show BuildList where
  show = genericShow
instance decodeBuildList :: Decode BuildList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildList :: Encode BuildList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildStatus = BuildStatus String
derive instance newtypeBuildStatus :: Newtype BuildStatus _
derive instance repGenericBuildStatus :: Generic BuildStatus _
instance showBuildStatus :: Show BuildStatus where
  show = genericShow
instance decodeBuildStatus :: Decode BuildStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildStatus :: Encode BuildStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComparisonOperatorType = ComparisonOperatorType String
derive instance newtypeComparisonOperatorType :: Newtype ComparisonOperatorType _
derive instance repGenericComparisonOperatorType :: Generic ComparisonOperatorType _
instance showComparisonOperatorType :: Show ComparisonOperatorType where
  show = genericShow
instance decodeComparisonOperatorType :: Decode ComparisonOperatorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparisonOperatorType :: Encode ComparisonOperatorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested operation would cause a conflict with the current state of a service resource associated with the request. Resolve the conflict before retrying this request.</p>
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeConflictException :: Newtype ConflictException _
derive instance repGenericConflictException :: Generic ConflictException _
instance showConflictException :: Show ConflictException where
  show = genericShow
instance decodeConflictException :: Decode ConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictException :: Encode ConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateAliasInput = CreateAliasInput 
  { "Name" :: (NonBlankAndLengthConstraintString)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "RoutingStrategy" :: (RoutingStrategy)
  }
derive instance newtypeCreateAliasInput :: Newtype CreateAliasInput _
derive instance repGenericCreateAliasInput :: Generic CreateAliasInput _
instance showCreateAliasInput :: Show CreateAliasInput where
  show = genericShow
instance decodeCreateAliasInput :: Decode CreateAliasInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasInput :: Encode CreateAliasInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateAliasOutput = CreateAliasOutput 
  { "Alias" :: NullOrUndefined.NullOrUndefined (Alias)
  }
derive instance newtypeCreateAliasOutput :: Newtype CreateAliasOutput _
derive instance repGenericCreateAliasOutput :: Generic CreateAliasOutput _
instance showCreateAliasOutput :: Show CreateAliasOutput where
  show = genericShow
instance decodeCreateAliasOutput :: Decode CreateAliasOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasOutput :: Encode CreateAliasOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateBuildInput = CreateBuildInput 
  { "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Version" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "StorageLocation" :: NullOrUndefined.NullOrUndefined (S3Location)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  }
derive instance newtypeCreateBuildInput :: Newtype CreateBuildInput _
derive instance repGenericCreateBuildInput :: Generic CreateBuildInput _
instance showCreateBuildInput :: Show CreateBuildInput where
  show = genericShow
instance decodeCreateBuildInput :: Decode CreateBuildInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBuildInput :: Encode CreateBuildInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateBuildOutput = CreateBuildOutput 
  { "Build" :: NullOrUndefined.NullOrUndefined (Build)
  , "UploadCredentials" :: NullOrUndefined.NullOrUndefined (AwsCredentials)
  , "StorageLocation" :: NullOrUndefined.NullOrUndefined (S3Location)
  }
derive instance newtypeCreateBuildOutput :: Newtype CreateBuildOutput _
derive instance repGenericCreateBuildOutput :: Generic CreateBuildOutput _
instance showCreateBuildOutput :: Show CreateBuildOutput where
  show = genericShow
instance decodeCreateBuildOutput :: Decode CreateBuildOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBuildOutput :: Encode CreateBuildOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateFleetInput = CreateFleetInput 
  { "Name" :: (NonZeroAndMaxString)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "BuildId" :: (BuildId)
  , "ServerLaunchPath" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "ServerLaunchParameters" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "LogPaths" :: NullOrUndefined.NullOrUndefined (StringList)
  , "EC2InstanceType" :: (EC2InstanceType)
  , "EC2InboundPermissions" :: NullOrUndefined.NullOrUndefined (IpPermissionsList)
  , "NewGameSessionProtectionPolicy" :: NullOrUndefined.NullOrUndefined (ProtectionPolicy)
  , "RuntimeConfiguration" :: NullOrUndefined.NullOrUndefined (RuntimeConfiguration)
  , "ResourceCreationLimitPolicy" :: NullOrUndefined.NullOrUndefined (ResourceCreationLimitPolicy)
  , "MetricGroups" :: NullOrUndefined.NullOrUndefined (MetricGroupList)
  , "PeerVpcAwsAccountId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PeerVpcId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "FleetType" :: NullOrUndefined.NullOrUndefined (FleetType)
  }
derive instance newtypeCreateFleetInput :: Newtype CreateFleetInput _
derive instance repGenericCreateFleetInput :: Generic CreateFleetInput _
instance showCreateFleetInput :: Show CreateFleetInput where
  show = genericShow
instance decodeCreateFleetInput :: Decode CreateFleetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFleetInput :: Encode CreateFleetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateFleetOutput = CreateFleetOutput 
  { "FleetAttributes" :: NullOrUndefined.NullOrUndefined (FleetAttributes)
  }
derive instance newtypeCreateFleetOutput :: Newtype CreateFleetOutput _
derive instance repGenericCreateFleetOutput :: Generic CreateFleetOutput _
instance showCreateFleetOutput :: Show CreateFleetOutput where
  show = genericShow
instance decodeCreateFleetOutput :: Decode CreateFleetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFleetOutput :: Encode CreateFleetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateGameSessionInput = CreateGameSessionInput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "AliasId" :: NullOrUndefined.NullOrUndefined (AliasId)
  , "MaximumPlayerSessionCount" :: (WholeNumber)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "CreatorId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionId" :: NullOrUndefined.NullOrUndefined (IdStringModel)
  , "IdempotencyToken" :: NullOrUndefined.NullOrUndefined (IdStringModel)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  }
derive instance newtypeCreateGameSessionInput :: Newtype CreateGameSessionInput _
derive instance repGenericCreateGameSessionInput :: Generic CreateGameSessionInput _
instance showCreateGameSessionInput :: Show CreateGameSessionInput where
  show = genericShow
instance decodeCreateGameSessionInput :: Decode CreateGameSessionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGameSessionInput :: Encode CreateGameSessionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateGameSessionOutput = CreateGameSessionOutput 
  { "GameSession" :: NullOrUndefined.NullOrUndefined (GameSession)
  }
derive instance newtypeCreateGameSessionOutput :: Newtype CreateGameSessionOutput _
derive instance repGenericCreateGameSessionOutput :: Generic CreateGameSessionOutput _
instance showCreateGameSessionOutput :: Show CreateGameSessionOutput where
  show = genericShow
instance decodeCreateGameSessionOutput :: Decode CreateGameSessionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGameSessionOutput :: Encode CreateGameSessionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateGameSessionQueueInput = CreateGameSessionQueueInput 
  { "Name" :: (GameSessionQueueName)
  , "TimeoutInSeconds" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "PlayerLatencyPolicies" :: NullOrUndefined.NullOrUndefined (PlayerLatencyPolicyList)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (GameSessionQueueDestinationList)
  }
derive instance newtypeCreateGameSessionQueueInput :: Newtype CreateGameSessionQueueInput _
derive instance repGenericCreateGameSessionQueueInput :: Generic CreateGameSessionQueueInput _
instance showCreateGameSessionQueueInput :: Show CreateGameSessionQueueInput where
  show = genericShow
instance decodeCreateGameSessionQueueInput :: Decode CreateGameSessionQueueInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGameSessionQueueInput :: Encode CreateGameSessionQueueInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateGameSessionQueueOutput = CreateGameSessionQueueOutput 
  { "GameSessionQueue" :: NullOrUndefined.NullOrUndefined (GameSessionQueue)
  }
derive instance newtypeCreateGameSessionQueueOutput :: Newtype CreateGameSessionQueueOutput _
derive instance repGenericCreateGameSessionQueueOutput :: Generic CreateGameSessionQueueOutput _
instance showCreateGameSessionQueueOutput :: Show CreateGameSessionQueueOutput where
  show = genericShow
instance decodeCreateGameSessionQueueOutput :: Decode CreateGameSessionQueueOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGameSessionQueueOutput :: Encode CreateGameSessionQueueOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateMatchmakingConfigurationInput = CreateMatchmakingConfigurationInput 
  { "Name" :: (MatchmakingIdStringModel)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionQueueArns" :: (QueueArnsList)
  , "RequestTimeoutSeconds" :: (MatchmakingRequestTimeoutInteger)
  , "AcceptanceTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (MatchmakingAcceptanceTimeoutInteger)
  , "AcceptanceRequired" :: (BooleanModel)
  , "RuleSetName" :: (MatchmakingIdStringModel)
  , "NotificationTarget" :: NullOrUndefined.NullOrUndefined (SnsArnStringModel)
  , "AdditionalPlayerCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "CustomEventData" :: NullOrUndefined.NullOrUndefined (CustomEventData)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  }
derive instance newtypeCreateMatchmakingConfigurationInput :: Newtype CreateMatchmakingConfigurationInput _
derive instance repGenericCreateMatchmakingConfigurationInput :: Generic CreateMatchmakingConfigurationInput _
instance showCreateMatchmakingConfigurationInput :: Show CreateMatchmakingConfigurationInput where
  show = genericShow
instance decodeCreateMatchmakingConfigurationInput :: Decode CreateMatchmakingConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMatchmakingConfigurationInput :: Encode CreateMatchmakingConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateMatchmakingConfigurationOutput = CreateMatchmakingConfigurationOutput 
  { "Configuration" :: NullOrUndefined.NullOrUndefined (MatchmakingConfiguration)
  }
derive instance newtypeCreateMatchmakingConfigurationOutput :: Newtype CreateMatchmakingConfigurationOutput _
derive instance repGenericCreateMatchmakingConfigurationOutput :: Generic CreateMatchmakingConfigurationOutput _
instance showCreateMatchmakingConfigurationOutput :: Show CreateMatchmakingConfigurationOutput where
  show = genericShow
instance decodeCreateMatchmakingConfigurationOutput :: Decode CreateMatchmakingConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMatchmakingConfigurationOutput :: Encode CreateMatchmakingConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateMatchmakingRuleSetInput = CreateMatchmakingRuleSetInput 
  { "Name" :: (MatchmakingIdStringModel)
  , "RuleSetBody" :: (RuleSetBody)
  }
derive instance newtypeCreateMatchmakingRuleSetInput :: Newtype CreateMatchmakingRuleSetInput _
derive instance repGenericCreateMatchmakingRuleSetInput :: Generic CreateMatchmakingRuleSetInput _
instance showCreateMatchmakingRuleSetInput :: Show CreateMatchmakingRuleSetInput where
  show = genericShow
instance decodeCreateMatchmakingRuleSetInput :: Decode CreateMatchmakingRuleSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMatchmakingRuleSetInput :: Encode CreateMatchmakingRuleSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateMatchmakingRuleSetOutput = CreateMatchmakingRuleSetOutput 
  { "RuleSet" :: (MatchmakingRuleSet)
  }
derive instance newtypeCreateMatchmakingRuleSetOutput :: Newtype CreateMatchmakingRuleSetOutput _
derive instance repGenericCreateMatchmakingRuleSetOutput :: Generic CreateMatchmakingRuleSetOutput _
instance showCreateMatchmakingRuleSetOutput :: Show CreateMatchmakingRuleSetOutput where
  show = genericShow
instance decodeCreateMatchmakingRuleSetOutput :: Decode CreateMatchmakingRuleSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMatchmakingRuleSetOutput :: Encode CreateMatchmakingRuleSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreatePlayerSessionInput = CreatePlayerSessionInput 
  { "GameSessionId" :: (ArnStringModel)
  , "PlayerId" :: (NonZeroAndMaxString)
  , "PlayerData" :: NullOrUndefined.NullOrUndefined (PlayerData)
  }
derive instance newtypeCreatePlayerSessionInput :: Newtype CreatePlayerSessionInput _
derive instance repGenericCreatePlayerSessionInput :: Generic CreatePlayerSessionInput _
instance showCreatePlayerSessionInput :: Show CreatePlayerSessionInput where
  show = genericShow
instance decodeCreatePlayerSessionInput :: Decode CreatePlayerSessionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlayerSessionInput :: Encode CreatePlayerSessionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreatePlayerSessionOutput = CreatePlayerSessionOutput 
  { "PlayerSession" :: NullOrUndefined.NullOrUndefined (PlayerSession)
  }
derive instance newtypeCreatePlayerSessionOutput :: Newtype CreatePlayerSessionOutput _
derive instance repGenericCreatePlayerSessionOutput :: Generic CreatePlayerSessionOutput _
instance showCreatePlayerSessionOutput :: Show CreatePlayerSessionOutput where
  show = genericShow
instance decodeCreatePlayerSessionOutput :: Decode CreatePlayerSessionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlayerSessionOutput :: Encode CreatePlayerSessionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreatePlayerSessionsInput = CreatePlayerSessionsInput 
  { "GameSessionId" :: (ArnStringModel)
  , "PlayerIds" :: (PlayerIdList)
  , "PlayerDataMap" :: NullOrUndefined.NullOrUndefined (PlayerDataMap)
  }
derive instance newtypeCreatePlayerSessionsInput :: Newtype CreatePlayerSessionsInput _
derive instance repGenericCreatePlayerSessionsInput :: Generic CreatePlayerSessionsInput _
instance showCreatePlayerSessionsInput :: Show CreatePlayerSessionsInput where
  show = genericShow
instance decodeCreatePlayerSessionsInput :: Decode CreatePlayerSessionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlayerSessionsInput :: Encode CreatePlayerSessionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreatePlayerSessionsOutput = CreatePlayerSessionsOutput 
  { "PlayerSessions" :: NullOrUndefined.NullOrUndefined (PlayerSessionList)
  }
derive instance newtypeCreatePlayerSessionsOutput :: Newtype CreatePlayerSessionsOutput _
derive instance repGenericCreatePlayerSessionsOutput :: Generic CreatePlayerSessionsOutput _
instance showCreatePlayerSessionsOutput :: Show CreatePlayerSessionsOutput where
  show = genericShow
instance decodeCreatePlayerSessionsOutput :: Decode CreatePlayerSessionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlayerSessionsOutput :: Encode CreatePlayerSessionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateVpcPeeringAuthorizationInput = CreateVpcPeeringAuthorizationInput 
  { "GameLiftAwsAccountId" :: (NonZeroAndMaxString)
  , "PeerVpcId" :: (NonZeroAndMaxString)
  }
derive instance newtypeCreateVpcPeeringAuthorizationInput :: Newtype CreateVpcPeeringAuthorizationInput _
derive instance repGenericCreateVpcPeeringAuthorizationInput :: Generic CreateVpcPeeringAuthorizationInput _
instance showCreateVpcPeeringAuthorizationInput :: Show CreateVpcPeeringAuthorizationInput where
  show = genericShow
instance decodeCreateVpcPeeringAuthorizationInput :: Decode CreateVpcPeeringAuthorizationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVpcPeeringAuthorizationInput :: Encode CreateVpcPeeringAuthorizationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype CreateVpcPeeringAuthorizationOutput = CreateVpcPeeringAuthorizationOutput 
  { "VpcPeeringAuthorization" :: NullOrUndefined.NullOrUndefined (VpcPeeringAuthorization)
  }
derive instance newtypeCreateVpcPeeringAuthorizationOutput :: Newtype CreateVpcPeeringAuthorizationOutput _
derive instance repGenericCreateVpcPeeringAuthorizationOutput :: Generic CreateVpcPeeringAuthorizationOutput _
instance showCreateVpcPeeringAuthorizationOutput :: Show CreateVpcPeeringAuthorizationOutput where
  show = genericShow
instance decodeCreateVpcPeeringAuthorizationOutput :: Decode CreateVpcPeeringAuthorizationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVpcPeeringAuthorizationOutput :: Encode CreateVpcPeeringAuthorizationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype CreateVpcPeeringConnectionInput = CreateVpcPeeringConnectionInput 
  { "FleetId" :: (FleetId)
  , "PeerVpcAwsAccountId" :: (NonZeroAndMaxString)
  , "PeerVpcId" :: (NonZeroAndMaxString)
  }
derive instance newtypeCreateVpcPeeringConnectionInput :: Newtype CreateVpcPeeringConnectionInput _
derive instance repGenericCreateVpcPeeringConnectionInput :: Generic CreateVpcPeeringConnectionInput _
instance showCreateVpcPeeringConnectionInput :: Show CreateVpcPeeringConnectionInput where
  show = genericShow
instance decodeCreateVpcPeeringConnectionInput :: Decode CreateVpcPeeringConnectionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVpcPeeringConnectionInput :: Encode CreateVpcPeeringConnectionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateVpcPeeringConnectionOutput = CreateVpcPeeringConnectionOutput Types.NoArguments
derive instance newtypeCreateVpcPeeringConnectionOutput :: Newtype CreateVpcPeeringConnectionOutput _
derive instance repGenericCreateVpcPeeringConnectionOutput :: Generic CreateVpcPeeringConnectionOutput _
instance showCreateVpcPeeringConnectionOutput :: Show CreateVpcPeeringConnectionOutput where
  show = genericShow
instance decodeCreateVpcPeeringConnectionOutput :: Decode CreateVpcPeeringConnectionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVpcPeeringConnectionOutput :: Encode CreateVpcPeeringConnectionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomEventData = CustomEventData String
derive instance newtypeCustomEventData :: Newtype CustomEventData _
derive instance repGenericCustomEventData :: Generic CustomEventData _
instance showCustomEventData :: Show CustomEventData where
  show = genericShow
instance decodeCustomEventData :: Decode CustomEventData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomEventData :: Encode CustomEventData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteAliasInput = DeleteAliasInput 
  { "AliasId" :: (AliasId)
  }
derive instance newtypeDeleteAliasInput :: Newtype DeleteAliasInput _
derive instance repGenericDeleteAliasInput :: Generic DeleteAliasInput _
instance showDeleteAliasInput :: Show DeleteAliasInput where
  show = genericShow
instance decodeDeleteAliasInput :: Decode DeleteAliasInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAliasInput :: Encode DeleteAliasInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteBuildInput = DeleteBuildInput 
  { "BuildId" :: (BuildId)
  }
derive instance newtypeDeleteBuildInput :: Newtype DeleteBuildInput _
derive instance repGenericDeleteBuildInput :: Generic DeleteBuildInput _
instance showDeleteBuildInput :: Show DeleteBuildInput where
  show = genericShow
instance decodeDeleteBuildInput :: Decode DeleteBuildInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBuildInput :: Encode DeleteBuildInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteFleetInput = DeleteFleetInput 
  { "FleetId" :: (FleetId)
  }
derive instance newtypeDeleteFleetInput :: Newtype DeleteFleetInput _
derive instance repGenericDeleteFleetInput :: Generic DeleteFleetInput _
instance showDeleteFleetInput :: Show DeleteFleetInput where
  show = genericShow
instance decodeDeleteFleetInput :: Decode DeleteFleetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFleetInput :: Encode DeleteFleetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteGameSessionQueueInput = DeleteGameSessionQueueInput 
  { "Name" :: (GameSessionQueueName)
  }
derive instance newtypeDeleteGameSessionQueueInput :: Newtype DeleteGameSessionQueueInput _
derive instance repGenericDeleteGameSessionQueueInput :: Generic DeleteGameSessionQueueInput _
instance showDeleteGameSessionQueueInput :: Show DeleteGameSessionQueueInput where
  show = genericShow
instance decodeDeleteGameSessionQueueInput :: Decode DeleteGameSessionQueueInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGameSessionQueueInput :: Encode DeleteGameSessionQueueInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGameSessionQueueOutput = DeleteGameSessionQueueOutput Types.NoArguments
derive instance newtypeDeleteGameSessionQueueOutput :: Newtype DeleteGameSessionQueueOutput _
derive instance repGenericDeleteGameSessionQueueOutput :: Generic DeleteGameSessionQueueOutput _
instance showDeleteGameSessionQueueOutput :: Show DeleteGameSessionQueueOutput where
  show = genericShow
instance decodeDeleteGameSessionQueueOutput :: Decode DeleteGameSessionQueueOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGameSessionQueueOutput :: Encode DeleteGameSessionQueueOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteMatchmakingConfigurationInput = DeleteMatchmakingConfigurationInput 
  { "Name" :: (MatchmakingIdStringModel)
  }
derive instance newtypeDeleteMatchmakingConfigurationInput :: Newtype DeleteMatchmakingConfigurationInput _
derive instance repGenericDeleteMatchmakingConfigurationInput :: Generic DeleteMatchmakingConfigurationInput _
instance showDeleteMatchmakingConfigurationInput :: Show DeleteMatchmakingConfigurationInput where
  show = genericShow
instance decodeDeleteMatchmakingConfigurationInput :: Decode DeleteMatchmakingConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMatchmakingConfigurationInput :: Encode DeleteMatchmakingConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMatchmakingConfigurationOutput = DeleteMatchmakingConfigurationOutput Types.NoArguments
derive instance newtypeDeleteMatchmakingConfigurationOutput :: Newtype DeleteMatchmakingConfigurationOutput _
derive instance repGenericDeleteMatchmakingConfigurationOutput :: Generic DeleteMatchmakingConfigurationOutput _
instance showDeleteMatchmakingConfigurationOutput :: Show DeleteMatchmakingConfigurationOutput where
  show = genericShow
instance decodeDeleteMatchmakingConfigurationOutput :: Decode DeleteMatchmakingConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMatchmakingConfigurationOutput :: Encode DeleteMatchmakingConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteScalingPolicyInput = DeleteScalingPolicyInput 
  { "Name" :: (NonZeroAndMaxString)
  , "FleetId" :: (FleetId)
  }
derive instance newtypeDeleteScalingPolicyInput :: Newtype DeleteScalingPolicyInput _
derive instance repGenericDeleteScalingPolicyInput :: Generic DeleteScalingPolicyInput _
instance showDeleteScalingPolicyInput :: Show DeleteScalingPolicyInput where
  show = genericShow
instance decodeDeleteScalingPolicyInput :: Decode DeleteScalingPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteScalingPolicyInput :: Encode DeleteScalingPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteVpcPeeringAuthorizationInput = DeleteVpcPeeringAuthorizationInput 
  { "GameLiftAwsAccountId" :: (NonZeroAndMaxString)
  , "PeerVpcId" :: (NonZeroAndMaxString)
  }
derive instance newtypeDeleteVpcPeeringAuthorizationInput :: Newtype DeleteVpcPeeringAuthorizationInput _
derive instance repGenericDeleteVpcPeeringAuthorizationInput :: Generic DeleteVpcPeeringAuthorizationInput _
instance showDeleteVpcPeeringAuthorizationInput :: Show DeleteVpcPeeringAuthorizationInput where
  show = genericShow
instance decodeDeleteVpcPeeringAuthorizationInput :: Decode DeleteVpcPeeringAuthorizationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVpcPeeringAuthorizationInput :: Encode DeleteVpcPeeringAuthorizationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteVpcPeeringAuthorizationOutput = DeleteVpcPeeringAuthorizationOutput Types.NoArguments
derive instance newtypeDeleteVpcPeeringAuthorizationOutput :: Newtype DeleteVpcPeeringAuthorizationOutput _
derive instance repGenericDeleteVpcPeeringAuthorizationOutput :: Generic DeleteVpcPeeringAuthorizationOutput _
instance showDeleteVpcPeeringAuthorizationOutput :: Show DeleteVpcPeeringAuthorizationOutput where
  show = genericShow
instance decodeDeleteVpcPeeringAuthorizationOutput :: Decode DeleteVpcPeeringAuthorizationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVpcPeeringAuthorizationOutput :: Encode DeleteVpcPeeringAuthorizationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DeleteVpcPeeringConnectionInput = DeleteVpcPeeringConnectionInput 
  { "FleetId" :: (FleetId)
  , "VpcPeeringConnectionId" :: (NonZeroAndMaxString)
  }
derive instance newtypeDeleteVpcPeeringConnectionInput :: Newtype DeleteVpcPeeringConnectionInput _
derive instance repGenericDeleteVpcPeeringConnectionInput :: Generic DeleteVpcPeeringConnectionInput _
instance showDeleteVpcPeeringConnectionInput :: Show DeleteVpcPeeringConnectionInput where
  show = genericShow
instance decodeDeleteVpcPeeringConnectionInput :: Decode DeleteVpcPeeringConnectionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVpcPeeringConnectionInput :: Encode DeleteVpcPeeringConnectionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteVpcPeeringConnectionOutput = DeleteVpcPeeringConnectionOutput Types.NoArguments
derive instance newtypeDeleteVpcPeeringConnectionOutput :: Newtype DeleteVpcPeeringConnectionOutput _
derive instance repGenericDeleteVpcPeeringConnectionOutput :: Generic DeleteVpcPeeringConnectionOutput _
instance showDeleteVpcPeeringConnectionOutput :: Show DeleteVpcPeeringConnectionOutput where
  show = genericShow
instance decodeDeleteVpcPeeringConnectionOutput :: Decode DeleteVpcPeeringConnectionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVpcPeeringConnectionOutput :: Encode DeleteVpcPeeringConnectionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeAliasInput = DescribeAliasInput 
  { "AliasId" :: (AliasId)
  }
derive instance newtypeDescribeAliasInput :: Newtype DescribeAliasInput _
derive instance repGenericDescribeAliasInput :: Generic DescribeAliasInput _
instance showDescribeAliasInput :: Show DescribeAliasInput where
  show = genericShow
instance decodeDescribeAliasInput :: Decode DescribeAliasInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAliasInput :: Encode DescribeAliasInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeAliasOutput = DescribeAliasOutput 
  { "Alias" :: NullOrUndefined.NullOrUndefined (Alias)
  }
derive instance newtypeDescribeAliasOutput :: Newtype DescribeAliasOutput _
derive instance repGenericDescribeAliasOutput :: Generic DescribeAliasOutput _
instance showDescribeAliasOutput :: Show DescribeAliasOutput where
  show = genericShow
instance decodeDescribeAliasOutput :: Decode DescribeAliasOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAliasOutput :: Encode DescribeAliasOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeBuildInput = DescribeBuildInput 
  { "BuildId" :: (BuildId)
  }
derive instance newtypeDescribeBuildInput :: Newtype DescribeBuildInput _
derive instance repGenericDescribeBuildInput :: Generic DescribeBuildInput _
instance showDescribeBuildInput :: Show DescribeBuildInput where
  show = genericShow
instance decodeDescribeBuildInput :: Decode DescribeBuildInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBuildInput :: Encode DescribeBuildInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeBuildOutput = DescribeBuildOutput 
  { "Build" :: NullOrUndefined.NullOrUndefined (Build)
  }
derive instance newtypeDescribeBuildOutput :: Newtype DescribeBuildOutput _
derive instance repGenericDescribeBuildOutput :: Generic DescribeBuildOutput _
instance showDescribeBuildOutput :: Show DescribeBuildOutput where
  show = genericShow
instance decodeDescribeBuildOutput :: Decode DescribeBuildOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBuildOutput :: Encode DescribeBuildOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeEC2InstanceLimitsInput = DescribeEC2InstanceLimitsInput 
  { "EC2InstanceType" :: NullOrUndefined.NullOrUndefined (EC2InstanceType)
  }
derive instance newtypeDescribeEC2InstanceLimitsInput :: Newtype DescribeEC2InstanceLimitsInput _
derive instance repGenericDescribeEC2InstanceLimitsInput :: Generic DescribeEC2InstanceLimitsInput _
instance showDescribeEC2InstanceLimitsInput :: Show DescribeEC2InstanceLimitsInput where
  show = genericShow
instance decodeDescribeEC2InstanceLimitsInput :: Decode DescribeEC2InstanceLimitsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEC2InstanceLimitsInput :: Encode DescribeEC2InstanceLimitsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeEC2InstanceLimitsOutput = DescribeEC2InstanceLimitsOutput 
  { "EC2InstanceLimits" :: NullOrUndefined.NullOrUndefined (EC2InstanceLimitList)
  }
derive instance newtypeDescribeEC2InstanceLimitsOutput :: Newtype DescribeEC2InstanceLimitsOutput _
derive instance repGenericDescribeEC2InstanceLimitsOutput :: Generic DescribeEC2InstanceLimitsOutput _
instance showDescribeEC2InstanceLimitsOutput :: Show DescribeEC2InstanceLimitsOutput where
  show = genericShow
instance decodeDescribeEC2InstanceLimitsOutput :: Decode DescribeEC2InstanceLimitsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEC2InstanceLimitsOutput :: Encode DescribeEC2InstanceLimitsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeFleetAttributesInput = DescribeFleetAttributesInput 
  { "FleetIds" :: NullOrUndefined.NullOrUndefined (FleetIdList)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetAttributesInput :: Newtype DescribeFleetAttributesInput _
derive instance repGenericDescribeFleetAttributesInput :: Generic DescribeFleetAttributesInput _
instance showDescribeFleetAttributesInput :: Show DescribeFleetAttributesInput where
  show = genericShow
instance decodeDescribeFleetAttributesInput :: Decode DescribeFleetAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetAttributesInput :: Encode DescribeFleetAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeFleetAttributesOutput = DescribeFleetAttributesOutput 
  { "FleetAttributes" :: NullOrUndefined.NullOrUndefined (FleetAttributesList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetAttributesOutput :: Newtype DescribeFleetAttributesOutput _
derive instance repGenericDescribeFleetAttributesOutput :: Generic DescribeFleetAttributesOutput _
instance showDescribeFleetAttributesOutput :: Show DescribeFleetAttributesOutput where
  show = genericShow
instance decodeDescribeFleetAttributesOutput :: Decode DescribeFleetAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetAttributesOutput :: Encode DescribeFleetAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeFleetCapacityInput = DescribeFleetCapacityInput 
  { "FleetIds" :: NullOrUndefined.NullOrUndefined (FleetIdList)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetCapacityInput :: Newtype DescribeFleetCapacityInput _
derive instance repGenericDescribeFleetCapacityInput :: Generic DescribeFleetCapacityInput _
instance showDescribeFleetCapacityInput :: Show DescribeFleetCapacityInput where
  show = genericShow
instance decodeDescribeFleetCapacityInput :: Decode DescribeFleetCapacityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetCapacityInput :: Encode DescribeFleetCapacityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeFleetCapacityOutput = DescribeFleetCapacityOutput 
  { "FleetCapacity" :: NullOrUndefined.NullOrUndefined (FleetCapacityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetCapacityOutput :: Newtype DescribeFleetCapacityOutput _
derive instance repGenericDescribeFleetCapacityOutput :: Generic DescribeFleetCapacityOutput _
instance showDescribeFleetCapacityOutput :: Show DescribeFleetCapacityOutput where
  show = genericShow
instance decodeDescribeFleetCapacityOutput :: Decode DescribeFleetCapacityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetCapacityOutput :: Encode DescribeFleetCapacityOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeFleetEventsInput = DescribeFleetEventsInput 
  { "FleetId" :: (FleetId)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetEventsInput :: Newtype DescribeFleetEventsInput _
derive instance repGenericDescribeFleetEventsInput :: Generic DescribeFleetEventsInput _
instance showDescribeFleetEventsInput :: Show DescribeFleetEventsInput where
  show = genericShow
instance decodeDescribeFleetEventsInput :: Decode DescribeFleetEventsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetEventsInput :: Encode DescribeFleetEventsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeFleetEventsOutput = DescribeFleetEventsOutput 
  { "Events" :: NullOrUndefined.NullOrUndefined (EventList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetEventsOutput :: Newtype DescribeFleetEventsOutput _
derive instance repGenericDescribeFleetEventsOutput :: Generic DescribeFleetEventsOutput _
instance showDescribeFleetEventsOutput :: Show DescribeFleetEventsOutput where
  show = genericShow
instance decodeDescribeFleetEventsOutput :: Decode DescribeFleetEventsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetEventsOutput :: Encode DescribeFleetEventsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeFleetPortSettingsInput = DescribeFleetPortSettingsInput 
  { "FleetId" :: (FleetId)
  }
derive instance newtypeDescribeFleetPortSettingsInput :: Newtype DescribeFleetPortSettingsInput _
derive instance repGenericDescribeFleetPortSettingsInput :: Generic DescribeFleetPortSettingsInput _
instance showDescribeFleetPortSettingsInput :: Show DescribeFleetPortSettingsInput where
  show = genericShow
instance decodeDescribeFleetPortSettingsInput :: Decode DescribeFleetPortSettingsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetPortSettingsInput :: Encode DescribeFleetPortSettingsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeFleetPortSettingsOutput = DescribeFleetPortSettingsOutput 
  { "InboundPermissions" :: NullOrUndefined.NullOrUndefined (IpPermissionsList)
  }
derive instance newtypeDescribeFleetPortSettingsOutput :: Newtype DescribeFleetPortSettingsOutput _
derive instance repGenericDescribeFleetPortSettingsOutput :: Generic DescribeFleetPortSettingsOutput _
instance showDescribeFleetPortSettingsOutput :: Show DescribeFleetPortSettingsOutput where
  show = genericShow
instance decodeDescribeFleetPortSettingsOutput :: Decode DescribeFleetPortSettingsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetPortSettingsOutput :: Encode DescribeFleetPortSettingsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeFleetUtilizationInput = DescribeFleetUtilizationInput 
  { "FleetIds" :: NullOrUndefined.NullOrUndefined (FleetIdList)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetUtilizationInput :: Newtype DescribeFleetUtilizationInput _
derive instance repGenericDescribeFleetUtilizationInput :: Generic DescribeFleetUtilizationInput _
instance showDescribeFleetUtilizationInput :: Show DescribeFleetUtilizationInput where
  show = genericShow
instance decodeDescribeFleetUtilizationInput :: Decode DescribeFleetUtilizationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetUtilizationInput :: Encode DescribeFleetUtilizationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeFleetUtilizationOutput = DescribeFleetUtilizationOutput 
  { "FleetUtilization" :: NullOrUndefined.NullOrUndefined (FleetUtilizationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeFleetUtilizationOutput :: Newtype DescribeFleetUtilizationOutput _
derive instance repGenericDescribeFleetUtilizationOutput :: Generic DescribeFleetUtilizationOutput _
instance showDescribeFleetUtilizationOutput :: Show DescribeFleetUtilizationOutput where
  show = genericShow
instance decodeDescribeFleetUtilizationOutput :: Decode DescribeFleetUtilizationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFleetUtilizationOutput :: Encode DescribeFleetUtilizationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeGameSessionDetailsInput = DescribeGameSessionDetailsInput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "GameSessionId" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "AliasId" :: NullOrUndefined.NullOrUndefined (AliasId)
  , "StatusFilter" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeGameSessionDetailsInput :: Newtype DescribeGameSessionDetailsInput _
derive instance repGenericDescribeGameSessionDetailsInput :: Generic DescribeGameSessionDetailsInput _
instance showDescribeGameSessionDetailsInput :: Show DescribeGameSessionDetailsInput where
  show = genericShow
instance decodeDescribeGameSessionDetailsInput :: Decode DescribeGameSessionDetailsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionDetailsInput :: Encode DescribeGameSessionDetailsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeGameSessionDetailsOutput = DescribeGameSessionDetailsOutput 
  { "GameSessionDetails" :: NullOrUndefined.NullOrUndefined (GameSessionDetailList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeGameSessionDetailsOutput :: Newtype DescribeGameSessionDetailsOutput _
derive instance repGenericDescribeGameSessionDetailsOutput :: Generic DescribeGameSessionDetailsOutput _
instance showDescribeGameSessionDetailsOutput :: Show DescribeGameSessionDetailsOutput where
  show = genericShow
instance decodeDescribeGameSessionDetailsOutput :: Decode DescribeGameSessionDetailsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionDetailsOutput :: Encode DescribeGameSessionDetailsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeGameSessionPlacementInput = DescribeGameSessionPlacementInput 
  { "PlacementId" :: (IdStringModel)
  }
derive instance newtypeDescribeGameSessionPlacementInput :: Newtype DescribeGameSessionPlacementInput _
derive instance repGenericDescribeGameSessionPlacementInput :: Generic DescribeGameSessionPlacementInput _
instance showDescribeGameSessionPlacementInput :: Show DescribeGameSessionPlacementInput where
  show = genericShow
instance decodeDescribeGameSessionPlacementInput :: Decode DescribeGameSessionPlacementInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionPlacementInput :: Encode DescribeGameSessionPlacementInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeGameSessionPlacementOutput = DescribeGameSessionPlacementOutput 
  { "GameSessionPlacement" :: NullOrUndefined.NullOrUndefined (GameSessionPlacement)
  }
derive instance newtypeDescribeGameSessionPlacementOutput :: Newtype DescribeGameSessionPlacementOutput _
derive instance repGenericDescribeGameSessionPlacementOutput :: Generic DescribeGameSessionPlacementOutput _
instance showDescribeGameSessionPlacementOutput :: Show DescribeGameSessionPlacementOutput where
  show = genericShow
instance decodeDescribeGameSessionPlacementOutput :: Decode DescribeGameSessionPlacementOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionPlacementOutput :: Encode DescribeGameSessionPlacementOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeGameSessionQueuesInput = DescribeGameSessionQueuesInput 
  { "Names" :: NullOrUndefined.NullOrUndefined (GameSessionQueueNameList)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeGameSessionQueuesInput :: Newtype DescribeGameSessionQueuesInput _
derive instance repGenericDescribeGameSessionQueuesInput :: Generic DescribeGameSessionQueuesInput _
instance showDescribeGameSessionQueuesInput :: Show DescribeGameSessionQueuesInput where
  show = genericShow
instance decodeDescribeGameSessionQueuesInput :: Decode DescribeGameSessionQueuesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionQueuesInput :: Encode DescribeGameSessionQueuesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeGameSessionQueuesOutput = DescribeGameSessionQueuesOutput 
  { "GameSessionQueues" :: NullOrUndefined.NullOrUndefined (GameSessionQueueList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeGameSessionQueuesOutput :: Newtype DescribeGameSessionQueuesOutput _
derive instance repGenericDescribeGameSessionQueuesOutput :: Generic DescribeGameSessionQueuesOutput _
instance showDescribeGameSessionQueuesOutput :: Show DescribeGameSessionQueuesOutput where
  show = genericShow
instance decodeDescribeGameSessionQueuesOutput :: Decode DescribeGameSessionQueuesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionQueuesOutput :: Encode DescribeGameSessionQueuesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeGameSessionsInput = DescribeGameSessionsInput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "GameSessionId" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "AliasId" :: NullOrUndefined.NullOrUndefined (AliasId)
  , "StatusFilter" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeGameSessionsInput :: Newtype DescribeGameSessionsInput _
derive instance repGenericDescribeGameSessionsInput :: Generic DescribeGameSessionsInput _
instance showDescribeGameSessionsInput :: Show DescribeGameSessionsInput where
  show = genericShow
instance decodeDescribeGameSessionsInput :: Decode DescribeGameSessionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionsInput :: Encode DescribeGameSessionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeGameSessionsOutput = DescribeGameSessionsOutput 
  { "GameSessions" :: NullOrUndefined.NullOrUndefined (GameSessionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeGameSessionsOutput :: Newtype DescribeGameSessionsOutput _
derive instance repGenericDescribeGameSessionsOutput :: Generic DescribeGameSessionsOutput _
instance showDescribeGameSessionsOutput :: Show DescribeGameSessionsOutput where
  show = genericShow
instance decodeDescribeGameSessionsOutput :: Decode DescribeGameSessionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGameSessionsOutput :: Encode DescribeGameSessionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeInstancesInput = DescribeInstancesInput 
  { "FleetId" :: (FleetId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeInstancesInput :: Newtype DescribeInstancesInput _
derive instance repGenericDescribeInstancesInput :: Generic DescribeInstancesInput _
instance showDescribeInstancesInput :: Show DescribeInstancesInput where
  show = genericShow
instance decodeDescribeInstancesInput :: Decode DescribeInstancesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancesInput :: Encode DescribeInstancesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeInstancesOutput = DescribeInstancesOutput 
  { "Instances" :: NullOrUndefined.NullOrUndefined (InstanceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeInstancesOutput :: Newtype DescribeInstancesOutput _
derive instance repGenericDescribeInstancesOutput :: Generic DescribeInstancesOutput _
instance showDescribeInstancesOutput :: Show DescribeInstancesOutput where
  show = genericShow
instance decodeDescribeInstancesOutput :: Decode DescribeInstancesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancesOutput :: Encode DescribeInstancesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeMatchmakingConfigurationsInput = DescribeMatchmakingConfigurationsInput 
  { "Names" :: NullOrUndefined.NullOrUndefined (MatchmakingIdList)
  , "RuleSetName" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeMatchmakingConfigurationsInput :: Newtype DescribeMatchmakingConfigurationsInput _
derive instance repGenericDescribeMatchmakingConfigurationsInput :: Generic DescribeMatchmakingConfigurationsInput _
instance showDescribeMatchmakingConfigurationsInput :: Show DescribeMatchmakingConfigurationsInput where
  show = genericShow
instance decodeDescribeMatchmakingConfigurationsInput :: Decode DescribeMatchmakingConfigurationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMatchmakingConfigurationsInput :: Encode DescribeMatchmakingConfigurationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeMatchmakingConfigurationsOutput = DescribeMatchmakingConfigurationsOutput 
  { "Configurations" :: NullOrUndefined.NullOrUndefined (MatchmakingConfigurationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeMatchmakingConfigurationsOutput :: Newtype DescribeMatchmakingConfigurationsOutput _
derive instance repGenericDescribeMatchmakingConfigurationsOutput :: Generic DescribeMatchmakingConfigurationsOutput _
instance showDescribeMatchmakingConfigurationsOutput :: Show DescribeMatchmakingConfigurationsOutput where
  show = genericShow
instance decodeDescribeMatchmakingConfigurationsOutput :: Decode DescribeMatchmakingConfigurationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMatchmakingConfigurationsOutput :: Encode DescribeMatchmakingConfigurationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeMatchmakingInput = DescribeMatchmakingInput 
  { "TicketIds" :: (MatchmakingIdList)
  }
derive instance newtypeDescribeMatchmakingInput :: Newtype DescribeMatchmakingInput _
derive instance repGenericDescribeMatchmakingInput :: Generic DescribeMatchmakingInput _
instance showDescribeMatchmakingInput :: Show DescribeMatchmakingInput where
  show = genericShow
instance decodeDescribeMatchmakingInput :: Decode DescribeMatchmakingInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMatchmakingInput :: Encode DescribeMatchmakingInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeMatchmakingOutput = DescribeMatchmakingOutput 
  { "TicketList" :: NullOrUndefined.NullOrUndefined (MatchmakingTicketList)
  }
derive instance newtypeDescribeMatchmakingOutput :: Newtype DescribeMatchmakingOutput _
derive instance repGenericDescribeMatchmakingOutput :: Generic DescribeMatchmakingOutput _
instance showDescribeMatchmakingOutput :: Show DescribeMatchmakingOutput where
  show = genericShow
instance decodeDescribeMatchmakingOutput :: Decode DescribeMatchmakingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMatchmakingOutput :: Encode DescribeMatchmakingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeMatchmakingRuleSetsInput = DescribeMatchmakingRuleSetsInput 
  { "Names" :: NullOrUndefined.NullOrUndefined (MatchmakingRuleSetNameList)
  , "Limit" :: NullOrUndefined.NullOrUndefined (RuleSetLimit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeMatchmakingRuleSetsInput :: Newtype DescribeMatchmakingRuleSetsInput _
derive instance repGenericDescribeMatchmakingRuleSetsInput :: Generic DescribeMatchmakingRuleSetsInput _
instance showDescribeMatchmakingRuleSetsInput :: Show DescribeMatchmakingRuleSetsInput where
  show = genericShow
instance decodeDescribeMatchmakingRuleSetsInput :: Decode DescribeMatchmakingRuleSetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMatchmakingRuleSetsInput :: Encode DescribeMatchmakingRuleSetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeMatchmakingRuleSetsOutput = DescribeMatchmakingRuleSetsOutput 
  { "RuleSets" :: (MatchmakingRuleSetList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeMatchmakingRuleSetsOutput :: Newtype DescribeMatchmakingRuleSetsOutput _
derive instance repGenericDescribeMatchmakingRuleSetsOutput :: Generic DescribeMatchmakingRuleSetsOutput _
instance showDescribeMatchmakingRuleSetsOutput :: Show DescribeMatchmakingRuleSetsOutput where
  show = genericShow
instance decodeDescribeMatchmakingRuleSetsOutput :: Decode DescribeMatchmakingRuleSetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMatchmakingRuleSetsOutput :: Encode DescribeMatchmakingRuleSetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribePlayerSessionsInput = DescribePlayerSessionsInput 
  { "GameSessionId" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerSessionId" :: NullOrUndefined.NullOrUndefined (PlayerSessionId)
  , "PlayerSessionStatusFilter" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribePlayerSessionsInput :: Newtype DescribePlayerSessionsInput _
derive instance repGenericDescribePlayerSessionsInput :: Generic DescribePlayerSessionsInput _
instance showDescribePlayerSessionsInput :: Show DescribePlayerSessionsInput where
  show = genericShow
instance decodeDescribePlayerSessionsInput :: Decode DescribePlayerSessionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePlayerSessionsInput :: Encode DescribePlayerSessionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribePlayerSessionsOutput = DescribePlayerSessionsOutput 
  { "PlayerSessions" :: NullOrUndefined.NullOrUndefined (PlayerSessionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribePlayerSessionsOutput :: Newtype DescribePlayerSessionsOutput _
derive instance repGenericDescribePlayerSessionsOutput :: Generic DescribePlayerSessionsOutput _
instance showDescribePlayerSessionsOutput :: Show DescribePlayerSessionsOutput where
  show = genericShow
instance decodeDescribePlayerSessionsOutput :: Decode DescribePlayerSessionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePlayerSessionsOutput :: Encode DescribePlayerSessionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeRuntimeConfigurationInput = DescribeRuntimeConfigurationInput 
  { "FleetId" :: (FleetId)
  }
derive instance newtypeDescribeRuntimeConfigurationInput :: Newtype DescribeRuntimeConfigurationInput _
derive instance repGenericDescribeRuntimeConfigurationInput :: Generic DescribeRuntimeConfigurationInput _
instance showDescribeRuntimeConfigurationInput :: Show DescribeRuntimeConfigurationInput where
  show = genericShow
instance decodeDescribeRuntimeConfigurationInput :: Decode DescribeRuntimeConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRuntimeConfigurationInput :: Encode DescribeRuntimeConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeRuntimeConfigurationOutput = DescribeRuntimeConfigurationOutput 
  { "RuntimeConfiguration" :: NullOrUndefined.NullOrUndefined (RuntimeConfiguration)
  }
derive instance newtypeDescribeRuntimeConfigurationOutput :: Newtype DescribeRuntimeConfigurationOutput _
derive instance repGenericDescribeRuntimeConfigurationOutput :: Generic DescribeRuntimeConfigurationOutput _
instance showDescribeRuntimeConfigurationOutput :: Show DescribeRuntimeConfigurationOutput where
  show = genericShow
instance decodeDescribeRuntimeConfigurationOutput :: Decode DescribeRuntimeConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRuntimeConfigurationOutput :: Encode DescribeRuntimeConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeScalingPoliciesInput = DescribeScalingPoliciesInput 
  { "FleetId" :: (FleetId)
  , "StatusFilter" :: NullOrUndefined.NullOrUndefined (ScalingStatusType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeScalingPoliciesInput :: Newtype DescribeScalingPoliciesInput _
derive instance repGenericDescribeScalingPoliciesInput :: Generic DescribeScalingPoliciesInput _
instance showDescribeScalingPoliciesInput :: Show DescribeScalingPoliciesInput where
  show = genericShow
instance decodeDescribeScalingPoliciesInput :: Decode DescribeScalingPoliciesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeScalingPoliciesInput :: Encode DescribeScalingPoliciesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeScalingPoliciesOutput = DescribeScalingPoliciesOutput 
  { "ScalingPolicies" :: NullOrUndefined.NullOrUndefined (ScalingPolicyList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeDescribeScalingPoliciesOutput :: Newtype DescribeScalingPoliciesOutput _
derive instance repGenericDescribeScalingPoliciesOutput :: Generic DescribeScalingPoliciesOutput _
instance showDescribeScalingPoliciesOutput :: Show DescribeScalingPoliciesOutput where
  show = genericShow
instance decodeDescribeScalingPoliciesOutput :: Decode DescribeScalingPoliciesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeScalingPoliciesOutput :: Encode DescribeScalingPoliciesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeVpcPeeringAuthorizationsInput = DescribeVpcPeeringAuthorizationsInput Types.NoArguments
derive instance newtypeDescribeVpcPeeringAuthorizationsInput :: Newtype DescribeVpcPeeringAuthorizationsInput _
derive instance repGenericDescribeVpcPeeringAuthorizationsInput :: Generic DescribeVpcPeeringAuthorizationsInput _
instance showDescribeVpcPeeringAuthorizationsInput :: Show DescribeVpcPeeringAuthorizationsInput where
  show = genericShow
instance decodeDescribeVpcPeeringAuthorizationsInput :: Decode DescribeVpcPeeringAuthorizationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVpcPeeringAuthorizationsInput :: Encode DescribeVpcPeeringAuthorizationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeVpcPeeringAuthorizationsOutput = DescribeVpcPeeringAuthorizationsOutput 
  { "VpcPeeringAuthorizations" :: NullOrUndefined.NullOrUndefined (VpcPeeringAuthorizationList)
  }
derive instance newtypeDescribeVpcPeeringAuthorizationsOutput :: Newtype DescribeVpcPeeringAuthorizationsOutput _
derive instance repGenericDescribeVpcPeeringAuthorizationsOutput :: Generic DescribeVpcPeeringAuthorizationsOutput _
instance showDescribeVpcPeeringAuthorizationsOutput :: Show DescribeVpcPeeringAuthorizationsOutput where
  show = genericShow
instance decodeDescribeVpcPeeringAuthorizationsOutput :: Decode DescribeVpcPeeringAuthorizationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVpcPeeringAuthorizationsOutput :: Encode DescribeVpcPeeringAuthorizationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype DescribeVpcPeeringConnectionsInput = DescribeVpcPeeringConnectionsInput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  }
derive instance newtypeDescribeVpcPeeringConnectionsInput :: Newtype DescribeVpcPeeringConnectionsInput _
derive instance repGenericDescribeVpcPeeringConnectionsInput :: Generic DescribeVpcPeeringConnectionsInput _
instance showDescribeVpcPeeringConnectionsInput :: Show DescribeVpcPeeringConnectionsInput where
  show = genericShow
instance decodeDescribeVpcPeeringConnectionsInput :: Decode DescribeVpcPeeringConnectionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVpcPeeringConnectionsInput :: Encode DescribeVpcPeeringConnectionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype DescribeVpcPeeringConnectionsOutput = DescribeVpcPeeringConnectionsOutput 
  { "VpcPeeringConnections" :: NullOrUndefined.NullOrUndefined (VpcPeeringConnectionList)
  }
derive instance newtypeDescribeVpcPeeringConnectionsOutput :: Newtype DescribeVpcPeeringConnectionsOutput _
derive instance repGenericDescribeVpcPeeringConnectionsOutput :: Generic DescribeVpcPeeringConnectionsOutput _
instance showDescribeVpcPeeringConnectionsOutput :: Show DescribeVpcPeeringConnectionsOutput where
  show = genericShow
instance decodeDescribeVpcPeeringConnectionsOutput :: Decode DescribeVpcPeeringConnectionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVpcPeeringConnectionsOutput :: Encode DescribeVpcPeeringConnectionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Player information for use when creating player sessions using a game session placement request with <a>StartGameSessionPlacement</a>.</p>
newtype DesiredPlayerSession = DesiredPlayerSession 
  { "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerData" :: NullOrUndefined.NullOrUndefined (PlayerData)
  }
derive instance newtypeDesiredPlayerSession :: Newtype DesiredPlayerSession _
derive instance repGenericDesiredPlayerSession :: Generic DesiredPlayerSession _
instance showDesiredPlayerSession :: Show DesiredPlayerSession where
  show = genericShow
instance decodeDesiredPlayerSession :: Decode DesiredPlayerSession where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDesiredPlayerSession :: Encode DesiredPlayerSession where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DesiredPlayerSessionList = DesiredPlayerSessionList (Array DesiredPlayerSession)
derive instance newtypeDesiredPlayerSessionList :: Newtype DesiredPlayerSessionList _
derive instance repGenericDesiredPlayerSessionList :: Generic DesiredPlayerSessionList _
instance showDesiredPlayerSessionList :: Show DesiredPlayerSessionList where
  show = genericShow
instance decodeDesiredPlayerSessionList :: Decode DesiredPlayerSessionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDesiredPlayerSessionList :: Encode DesiredPlayerSessionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DoubleObject = DoubleObject Number
derive instance newtypeDoubleObject :: Newtype DoubleObject _
derive instance repGenericDoubleObject :: Generic DoubleObject _
instance showDoubleObject :: Show DoubleObject where
  show = genericShow
instance decodeDoubleObject :: Decode DoubleObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDoubleObject :: Encode DoubleObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an <a>UpdateFleetCapacity</a> request, or if access to resources is temporarily affected.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype EC2InstanceCounts = EC2InstanceCounts 
  { "DESIRED" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "MINIMUM" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "MAXIMUM" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "PENDING" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "ACTIVE" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "IDLE" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "TERMINATING" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeEC2InstanceCounts :: Newtype EC2InstanceCounts _
derive instance repGenericEC2InstanceCounts :: Generic EC2InstanceCounts _
instance showEC2InstanceCounts :: Show EC2InstanceCounts where
  show = genericShow
instance decodeEC2InstanceCounts :: Decode EC2InstanceCounts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceCounts :: Encode EC2InstanceCounts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling <a>DescribeEC2InstanceLimits</a>.</p>
newtype EC2InstanceLimit = EC2InstanceLimit 
  { "EC2InstanceType" :: NullOrUndefined.NullOrUndefined (EC2InstanceType)
  , "CurrentInstances" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "InstanceLimit" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeEC2InstanceLimit :: Newtype EC2InstanceLimit _
derive instance repGenericEC2InstanceLimit :: Generic EC2InstanceLimit _
instance showEC2InstanceLimit :: Show EC2InstanceLimit where
  show = genericShow
instance decodeEC2InstanceLimit :: Decode EC2InstanceLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceLimit :: Encode EC2InstanceLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2InstanceLimitList = EC2InstanceLimitList (Array EC2InstanceLimit)
derive instance newtypeEC2InstanceLimitList :: Newtype EC2InstanceLimitList _
derive instance repGenericEC2InstanceLimitList :: Generic EC2InstanceLimitList _
instance showEC2InstanceLimitList :: Show EC2InstanceLimitList where
  show = genericShow
instance decodeEC2InstanceLimitList :: Decode EC2InstanceLimitList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceLimitList :: Encode EC2InstanceLimitList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2InstanceType = EC2InstanceType String
derive instance newtypeEC2InstanceType :: Newtype EC2InstanceType _
derive instance repGenericEC2InstanceType :: Generic EC2InstanceType _
instance showEC2InstanceType :: Show EC2InstanceType where
  show = genericShow
instance decodeEC2InstanceType :: Decode EC2InstanceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceType :: Encode EC2InstanceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Log entry describing an event that involves Amazon GameLift resources (such as a fleet). In addition to tracking activity, event codes and messages can provide additional information for troubleshooting and debugging problems.</p>
newtype Event = Event 
  { "EventId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "EventCode" :: NullOrUndefined.NullOrUndefined (EventCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "EventTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "PreSignedLogUrl" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventCode = EventCode String
derive instance newtypeEventCode :: Newtype EventCode _
derive instance repGenericEventCode :: Generic EventCode _
instance showEventCode :: Show EventCode where
  show = genericShow
instance decodeEventCode :: Decode EventCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventCode :: Encode EventCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _
derive instance repGenericEventList :: Generic EventList _
instance showEventList :: Show EventList where
  show = genericShow
instance decodeEventList :: Decode EventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventList :: Encode EventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>General properties describing a fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype FleetAttributes = FleetAttributes 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "FleetArn" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "FleetType" :: NullOrUndefined.NullOrUndefined (FleetType)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (EC2InstanceType)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "TerminationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Status" :: NullOrUndefined.NullOrUndefined (FleetStatus)
  , "BuildId" :: NullOrUndefined.NullOrUndefined (BuildId)
  , "ServerLaunchPath" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "ServerLaunchParameters" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "LogPaths" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NewGameSessionProtectionPolicy" :: NullOrUndefined.NullOrUndefined (ProtectionPolicy)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "ResourceCreationLimitPolicy" :: NullOrUndefined.NullOrUndefined (ResourceCreationLimitPolicy)
  , "MetricGroups" :: NullOrUndefined.NullOrUndefined (MetricGroupList)
  }
derive instance newtypeFleetAttributes :: Newtype FleetAttributes _
derive instance repGenericFleetAttributes :: Generic FleetAttributes _
instance showFleetAttributes :: Show FleetAttributes where
  show = genericShow
instance decodeFleetAttributes :: Decode FleetAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetAttributes :: Encode FleetAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetAttributesList = FleetAttributesList (Array FleetAttributes)
derive instance newtypeFleetAttributesList :: Newtype FleetAttributesList _
derive instance repGenericFleetAttributesList :: Generic FleetAttributesList _
instance showFleetAttributesList :: Show FleetAttributesList where
  show = genericShow
instance decodeFleetAttributesList :: Decode FleetAttributesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetAttributesList :: Encode FleetAttributesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the fleet's capacity. Fleet capacity is measured in EC2 instances. By default, new fleets have a capacity of one instance, but can be updated as needed. The maximum number of instances for a fleet is determined by the fleet's instance type.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype FleetCapacity = FleetCapacity 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (EC2InstanceType)
  , "InstanceCounts" :: NullOrUndefined.NullOrUndefined (EC2InstanceCounts)
  }
derive instance newtypeFleetCapacity :: Newtype FleetCapacity _
derive instance repGenericFleetCapacity :: Generic FleetCapacity _
instance showFleetCapacity :: Show FleetCapacity where
  show = genericShow
instance decodeFleetCapacity :: Decode FleetCapacity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetCapacity :: Encode FleetCapacity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified fleet has no available instances to fulfill a <code>CreateGameSession</code> request. Clients can retry such requests immediately or after a waiting period.</p>
newtype FleetCapacityExceededException = FleetCapacityExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeFleetCapacityExceededException :: Newtype FleetCapacityExceededException _
derive instance repGenericFleetCapacityExceededException :: Generic FleetCapacityExceededException _
instance showFleetCapacityExceededException :: Show FleetCapacityExceededException where
  show = genericShow
instance decodeFleetCapacityExceededException :: Decode FleetCapacityExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetCapacityExceededException :: Encode FleetCapacityExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetCapacityList = FleetCapacityList (Array FleetCapacity)
derive instance newtypeFleetCapacityList :: Newtype FleetCapacityList _
derive instance repGenericFleetCapacityList :: Generic FleetCapacityList _
instance showFleetCapacityList :: Show FleetCapacityList where
  show = genericShow
instance decodeFleetCapacityList :: Decode FleetCapacityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetCapacityList :: Encode FleetCapacityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetId = FleetId String
derive instance newtypeFleetId :: Newtype FleetId _
derive instance repGenericFleetId :: Generic FleetId _
instance showFleetId :: Show FleetId where
  show = genericShow
instance decodeFleetId :: Decode FleetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetId :: Encode FleetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetIdList = FleetIdList (Array FleetId)
derive instance newtypeFleetIdList :: Newtype FleetIdList _
derive instance repGenericFleetIdList :: Generic FleetIdList _
instance showFleetIdList :: Show FleetIdList where
  show = genericShow
instance decodeFleetIdList :: Decode FleetIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetIdList :: Encode FleetIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetStatus = FleetStatus String
derive instance newtypeFleetStatus :: Newtype FleetStatus _
derive instance repGenericFleetStatus :: Generic FleetStatus _
instance showFleetStatus :: Show FleetStatus where
  show = genericShow
instance decodeFleetStatus :: Decode FleetStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetStatus :: Encode FleetStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetType = FleetType String
derive instance newtypeFleetType :: Newtype FleetType _
derive instance repGenericFleetType :: Generic FleetType _
instance showFleetType :: Show FleetType where
  show = genericShow
instance decodeFleetType :: Decode FleetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetType :: Encode FleetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Current status of fleet utilization, including the number of game and player sessions being hosted.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype FleetUtilization = FleetUtilization 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "ActiveServerProcessCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "ActiveGameSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "CurrentPlayerSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "MaximumPlayerSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeFleetUtilization :: Newtype FleetUtilization _
derive instance repGenericFleetUtilization :: Generic FleetUtilization _
instance showFleetUtilization :: Show FleetUtilization where
  show = genericShow
instance decodeFleetUtilization :: Decode FleetUtilization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetUtilization :: Encode FleetUtilization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FleetUtilizationList = FleetUtilizationList (Array FleetUtilization)
derive instance newtypeFleetUtilizationList :: Newtype FleetUtilizationList _
derive instance repGenericFleetUtilizationList :: Generic FleetUtilizationList _
instance showFleetUtilizationList :: Show FleetUtilizationList where
  show = genericShow
instance decodeFleetUtilizationList :: Decode FleetUtilizationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFleetUtilizationList :: Encode FleetUtilizationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FreeText = FreeText String
derive instance newtypeFreeText :: Newtype FreeText _
derive instance repGenericFreeText :: Generic FreeText _
instance showFreeText :: Show FreeText where
  show = genericShow
instance decodeFreeText :: Decode FreeText where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFreeText :: Encode FreeText where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Set of key-value pairs that contain information about a game session. When included in a game session request, these properties communicate details to be used when setting up the new game session, such as to specify a game mode, level, or map. Game properties are passed to the game server process when initiating a new game session; the server process uses the properties as appropriate. For more information, see the <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-create"> Amazon GameLift Developer Guide</a>.</p>
newtype GameProperty = GameProperty 
  { "Key" :: (GamePropertyKey)
  , "Value" :: (GamePropertyValue)
  }
derive instance newtypeGameProperty :: Newtype GameProperty _
derive instance repGenericGameProperty :: Generic GameProperty _
instance showGameProperty :: Show GameProperty where
  show = genericShow
instance decodeGameProperty :: Decode GameProperty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameProperty :: Encode GameProperty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GamePropertyKey = GamePropertyKey String
derive instance newtypeGamePropertyKey :: Newtype GamePropertyKey _
derive instance repGenericGamePropertyKey :: Generic GamePropertyKey _
instance showGamePropertyKey :: Show GamePropertyKey where
  show = genericShow
instance decodeGamePropertyKey :: Decode GamePropertyKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGamePropertyKey :: Encode GamePropertyKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GamePropertyList = GamePropertyList (Array GameProperty)
derive instance newtypeGamePropertyList :: Newtype GamePropertyList _
derive instance repGenericGamePropertyList :: Generic GamePropertyList _
instance showGamePropertyList :: Show GamePropertyList where
  show = genericShow
instance decodeGamePropertyList :: Decode GamePropertyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGamePropertyList :: Encode GamePropertyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GamePropertyValue = GamePropertyValue String
derive instance newtypeGamePropertyValue :: Newtype GamePropertyValue _
derive instance repGenericGamePropertyValue :: Generic GamePropertyValue _
instance showGamePropertyValue :: Show GamePropertyValue where
  show = genericShow
instance decodeGamePropertyValue :: Decode GamePropertyValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGamePropertyValue :: Encode GamePropertyValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Properties describing a game session.</p> <p>A game session in ACTIVE status can host players. When a game session ends, its status is set to <code>TERMINATED</code>. </p> <p>Once the session ends, the game session object is retained for 30 days. This means you can reuse idempotency token values after this time. Game session logs are retained for 14 days.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
newtype GameSession = GameSession 
  { "GameSessionId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "TerminationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrentPlayerSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "MaximumPlayerSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "Status" :: NullOrUndefined.NullOrUndefined (GameSessionStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (GameSessionStatusReason)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "Port" :: NullOrUndefined.NullOrUndefined (PortNumber)
  , "PlayerSessionCreationPolicy" :: NullOrUndefined.NullOrUndefined (PlayerSessionCreationPolicy)
  , "CreatorId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  , "MatchmakerData" :: NullOrUndefined.NullOrUndefined (MatchmakerData)
  }
derive instance newtypeGameSession :: Newtype GameSession _
derive instance repGenericGameSession :: Generic GameSession _
instance showGameSession :: Show GameSession where
  show = genericShow
instance decodeGameSession :: Decode GameSession where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSession :: Encode GameSession where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionActivationTimeoutSeconds = GameSessionActivationTimeoutSeconds Int
derive instance newtypeGameSessionActivationTimeoutSeconds :: Newtype GameSessionActivationTimeoutSeconds _
derive instance repGenericGameSessionActivationTimeoutSeconds :: Generic GameSessionActivationTimeoutSeconds _
instance showGameSessionActivationTimeoutSeconds :: Show GameSessionActivationTimeoutSeconds where
  show = genericShow
instance decodeGameSessionActivationTimeoutSeconds :: Decode GameSessionActivationTimeoutSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionActivationTimeoutSeconds :: Encode GameSessionActivationTimeoutSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Connection information for the new game session that is created with matchmaking. (with <a>StartMatchmaking</a>). Once a match is set, the FlexMatch engine places the match and creates a new game session for it. This information, including the game session endpoint and player sessions for each player in the original matchmaking request, is added to the <a>MatchmakingTicket</a>, which can be retrieved by calling <a>DescribeMatchmaking</a>.</p>
newtype GameSessionConnectionInfo = GameSessionConnectionInfo 
  { "GameSessionArn" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (StringModel)
  , "Port" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "MatchedPlayerSessions" :: NullOrUndefined.NullOrUndefined (MatchedPlayerSessionList)
  }
derive instance newtypeGameSessionConnectionInfo :: Newtype GameSessionConnectionInfo _
derive instance repGenericGameSessionConnectionInfo :: Generic GameSessionConnectionInfo _
instance showGameSessionConnectionInfo :: Show GameSessionConnectionInfo where
  show = genericShow
instance decodeGameSessionConnectionInfo :: Decode GameSessionConnectionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionConnectionInfo :: Encode GameSessionConnectionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionData = GameSessionData String
derive instance newtypeGameSessionData :: Newtype GameSessionData _
derive instance repGenericGameSessionData :: Generic GameSessionData _
instance showGameSessionData :: Show GameSessionData where
  show = genericShow
instance decodeGameSessionData :: Decode GameSessionData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionData :: Encode GameSessionData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A game session's properties plus the protection policy currently in force.</p>
newtype GameSessionDetail = GameSessionDetail 
  { "GameSession" :: NullOrUndefined.NullOrUndefined (GameSession)
  , "ProtectionPolicy" :: NullOrUndefined.NullOrUndefined (ProtectionPolicy)
  }
derive instance newtypeGameSessionDetail :: Newtype GameSessionDetail _
derive instance repGenericGameSessionDetail :: Generic GameSessionDetail _
instance showGameSessionDetail :: Show GameSessionDetail where
  show = genericShow
instance decodeGameSessionDetail :: Decode GameSessionDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionDetail :: Encode GameSessionDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionDetailList = GameSessionDetailList (Array GameSessionDetail)
derive instance newtypeGameSessionDetailList :: Newtype GameSessionDetailList _
derive instance repGenericGameSessionDetailList :: Generic GameSessionDetailList _
instance showGameSessionDetailList :: Show GameSessionDetailList where
  show = genericShow
instance decodeGameSessionDetailList :: Decode GameSessionDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionDetailList :: Encode GameSessionDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The game instance is currently full and cannot allow the requested player(s) to join. Clients can retry such requests immediately or after a waiting period.</p>
newtype GameSessionFullException = GameSessionFullException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeGameSessionFullException :: Newtype GameSessionFullException _
derive instance repGenericGameSessionFullException :: Generic GameSessionFullException _
instance showGameSessionFullException :: Show GameSessionFullException where
  show = genericShow
instance decodeGameSessionFullException :: Decode GameSessionFullException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionFullException :: Encode GameSessionFullException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionList = GameSessionList (Array GameSession)
derive instance newtypeGameSessionList :: Newtype GameSessionList _
derive instance repGenericGameSessionList :: Generic GameSessionList _
instance showGameSessionList :: Show GameSessionList where
  show = genericShow
instance decodeGameSessionList :: Decode GameSessionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionList :: Encode GameSessionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Object that describes a <a>StartGameSessionPlacement</a> request. This object includes the full details of the original request plus the current status and start/end time stamps.</p> <p>Game session placement-related operations include:</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul>
newtype GameSessionPlacement = GameSessionPlacement 
  { "PlacementId" :: NullOrUndefined.NullOrUndefined (IdStringModel)
  , "GameSessionQueueName" :: NullOrUndefined.NullOrUndefined (GameSessionQueueName)
  , "Status" :: NullOrUndefined.NullOrUndefined (GameSessionPlacementState)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "MaximumPlayerSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "GameSessionName" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionArn" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionRegion" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerLatencies" :: NullOrUndefined.NullOrUndefined (PlayerLatencyList)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "Port" :: NullOrUndefined.NullOrUndefined (PortNumber)
  , "PlacedPlayerSessions" :: NullOrUndefined.NullOrUndefined (PlacedPlayerSessionList)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  , "MatchmakerData" :: NullOrUndefined.NullOrUndefined (MatchmakerData)
  }
derive instance newtypeGameSessionPlacement :: Newtype GameSessionPlacement _
derive instance repGenericGameSessionPlacement :: Generic GameSessionPlacement _
instance showGameSessionPlacement :: Show GameSessionPlacement where
  show = genericShow
instance decodeGameSessionPlacement :: Decode GameSessionPlacement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionPlacement :: Encode GameSessionPlacement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionPlacementState = GameSessionPlacementState String
derive instance newtypeGameSessionPlacementState :: Newtype GameSessionPlacementState _
derive instance repGenericGameSessionPlacementState :: Generic GameSessionPlacementState _
instance showGameSessionPlacementState :: Show GameSessionPlacementState where
  show = genericShow
instance decodeGameSessionPlacementState :: Decode GameSessionPlacementState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionPlacementState :: Encode GameSessionPlacementState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration of a queue that is used to process game session placement requests. The queue configuration identifies several game features:</p> <ul> <li> <p>The destinations where a new game session can potentially be hosted. Amazon GameLift tries these destinations in an order based on either the queue's default order or player latency information, if provided in a placement request. With latency information, Amazon GameLift can place game sessions where the majority of players are reporting the lowest possible latency. </p> </li> <li> <p>The length of time that placement requests can wait in the queue before timing out. </p> </li> <li> <p>A set of optional latency policies that protect individual players from high latencies, preventing game sessions from being placed where any individual player is reporting latency higher than a policy's maximum.</p> </li> </ul> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
newtype GameSessionQueue = GameSessionQueue 
  { "Name" :: NullOrUndefined.NullOrUndefined (GameSessionQueueName)
  , "GameSessionQueueArn" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  , "TimeoutInSeconds" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "PlayerLatencyPolicies" :: NullOrUndefined.NullOrUndefined (PlayerLatencyPolicyList)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (GameSessionQueueDestinationList)
  }
derive instance newtypeGameSessionQueue :: Newtype GameSessionQueue _
derive instance repGenericGameSessionQueue :: Generic GameSessionQueue _
instance showGameSessionQueue :: Show GameSessionQueue where
  show = genericShow
instance decodeGameSessionQueue :: Decode GameSessionQueue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionQueue :: Encode GameSessionQueue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Fleet designated in a game session queue. Requests for new game sessions in the queue are fulfilled by starting a new game session on any destination configured for a queue. </p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
newtype GameSessionQueueDestination = GameSessionQueueDestination 
  { "DestinationArn" :: NullOrUndefined.NullOrUndefined (ArnStringModel)
  }
derive instance newtypeGameSessionQueueDestination :: Newtype GameSessionQueueDestination _
derive instance repGenericGameSessionQueueDestination :: Generic GameSessionQueueDestination _
instance showGameSessionQueueDestination :: Show GameSessionQueueDestination where
  show = genericShow
instance decodeGameSessionQueueDestination :: Decode GameSessionQueueDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionQueueDestination :: Encode GameSessionQueueDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionQueueDestinationList = GameSessionQueueDestinationList (Array GameSessionQueueDestination)
derive instance newtypeGameSessionQueueDestinationList :: Newtype GameSessionQueueDestinationList _
derive instance repGenericGameSessionQueueDestinationList :: Generic GameSessionQueueDestinationList _
instance showGameSessionQueueDestinationList :: Show GameSessionQueueDestinationList where
  show = genericShow
instance decodeGameSessionQueueDestinationList :: Decode GameSessionQueueDestinationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionQueueDestinationList :: Encode GameSessionQueueDestinationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionQueueList = GameSessionQueueList (Array GameSessionQueue)
derive instance newtypeGameSessionQueueList :: Newtype GameSessionQueueList _
derive instance repGenericGameSessionQueueList :: Generic GameSessionQueueList _
instance showGameSessionQueueList :: Show GameSessionQueueList where
  show = genericShow
instance decodeGameSessionQueueList :: Decode GameSessionQueueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionQueueList :: Encode GameSessionQueueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionQueueName = GameSessionQueueName String
derive instance newtypeGameSessionQueueName :: Newtype GameSessionQueueName _
derive instance repGenericGameSessionQueueName :: Generic GameSessionQueueName _
instance showGameSessionQueueName :: Show GameSessionQueueName where
  show = genericShow
instance decodeGameSessionQueueName :: Decode GameSessionQueueName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionQueueName :: Encode GameSessionQueueName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionQueueNameList = GameSessionQueueNameList (Array GameSessionQueueName)
derive instance newtypeGameSessionQueueNameList :: Newtype GameSessionQueueNameList _
derive instance repGenericGameSessionQueueNameList :: Generic GameSessionQueueNameList _
instance showGameSessionQueueNameList :: Show GameSessionQueueNameList where
  show = genericShow
instance decodeGameSessionQueueNameList :: Decode GameSessionQueueNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionQueueNameList :: Encode GameSessionQueueNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionStatus = GameSessionStatus String
derive instance newtypeGameSessionStatus :: Newtype GameSessionStatus _
derive instance repGenericGameSessionStatus :: Generic GameSessionStatus _
instance showGameSessionStatus :: Show GameSessionStatus where
  show = genericShow
instance decodeGameSessionStatus :: Decode GameSessionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionStatus :: Encode GameSessionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GameSessionStatusReason = GameSessionStatusReason String
derive instance newtypeGameSessionStatusReason :: Newtype GameSessionStatusReason _
derive instance repGenericGameSessionStatusReason :: Generic GameSessionStatusReason _
instance showGameSessionStatusReason :: Show GameSessionStatusReason where
  show = genericShow
instance decodeGameSessionStatusReason :: Decode GameSessionStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGameSessionStatusReason :: Encode GameSessionStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype GetGameSessionLogUrlInput = GetGameSessionLogUrlInput 
  { "GameSessionId" :: (ArnStringModel)
  }
derive instance newtypeGetGameSessionLogUrlInput :: Newtype GetGameSessionLogUrlInput _
derive instance repGenericGetGameSessionLogUrlInput :: Generic GetGameSessionLogUrlInput _
instance showGetGameSessionLogUrlInput :: Show GetGameSessionLogUrlInput where
  show = genericShow
instance decodeGetGameSessionLogUrlInput :: Decode GetGameSessionLogUrlInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGameSessionLogUrlInput :: Encode GetGameSessionLogUrlInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype GetGameSessionLogUrlOutput = GetGameSessionLogUrlOutput 
  { "PreSignedUrl" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeGetGameSessionLogUrlOutput :: Newtype GetGameSessionLogUrlOutput _
derive instance repGenericGetGameSessionLogUrlOutput :: Generic GetGameSessionLogUrlOutput _
instance showGetGameSessionLogUrlOutput :: Show GetGameSessionLogUrlOutput where
  show = genericShow
instance decodeGetGameSessionLogUrlOutput :: Decode GetGameSessionLogUrlOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGameSessionLogUrlOutput :: Encode GetGameSessionLogUrlOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype GetInstanceAccessInput = GetInstanceAccessInput 
  { "FleetId" :: (FleetId)
  , "InstanceId" :: (InstanceId)
  }
derive instance newtypeGetInstanceAccessInput :: Newtype GetInstanceAccessInput _
derive instance repGenericGetInstanceAccessInput :: Generic GetInstanceAccessInput _
instance showGetInstanceAccessInput :: Show GetInstanceAccessInput where
  show = genericShow
instance decodeGetInstanceAccessInput :: Decode GetInstanceAccessInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceAccessInput :: Encode GetInstanceAccessInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype GetInstanceAccessOutput = GetInstanceAccessOutput 
  { "InstanceAccess" :: NullOrUndefined.NullOrUndefined (InstanceAccess)
  }
derive instance newtypeGetInstanceAccessOutput :: Newtype GetInstanceAccessOutput _
derive instance repGenericGetInstanceAccessOutput :: Generic GetInstanceAccessOutput _
instance showGetInstanceAccessOutput :: Show GetInstanceAccessOutput where
  show = genericShow
instance decodeGetInstanceAccessOutput :: Decode GetInstanceAccessOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInstanceAccessOutput :: Encode GetInstanceAccessOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdStringModel = IdStringModel String
derive instance newtypeIdStringModel :: Newtype IdStringModel _
derive instance repGenericIdStringModel :: Generic IdStringModel _
instance showIdStringModel :: Show IdStringModel where
  show = genericShow
instance decodeIdStringModel :: Decode IdStringModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdStringModel :: Encode IdStringModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A game session with this custom ID string already exists in this fleet. Resolve this conflict before retrying this request.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _
derive instance repGenericIdempotentParameterMismatchException :: Generic IdempotentParameterMismatchException _
instance showIdempotentParameterMismatchException :: Show IdempotentParameterMismatchException where
  show = genericShow
instance decodeIdempotentParameterMismatchException :: Decode IdempotentParameterMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotentParameterMismatchException :: Encode IdempotentParameterMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet may contain zero or more instances.</p>
newtype Instance = Instance 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "Type" :: NullOrUndefined.NullOrUndefined (EC2InstanceType)
  , "Status" :: NullOrUndefined.NullOrUndefined (InstanceStatus)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeInstance :: Newtype Instance _
derive instance repGenericInstance :: Generic Instance _
instance showInstance :: Show Instance where
  show = genericShow
instance decodeInstance :: Decode Instance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstance :: Encode Instance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information required to remotely connect to a fleet instance. Access is requested by calling <a>GetInstanceAccess</a>. </p>
newtype InstanceAccess = InstanceAccess 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "Credentials" :: NullOrUndefined.NullOrUndefined (InstanceCredentials)
  }
derive instance newtypeInstanceAccess :: Newtype InstanceAccess _
derive instance repGenericInstanceAccess :: Generic InstanceAccess _
instance showInstanceAccess :: Show InstanceAccess where
  show = genericShow
instance decodeInstanceAccess :: Decode InstanceAccess where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAccess :: Encode InstanceAccess where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling <a>GetInstanceAccess</a> and returned in an <a>InstanceAccess</a> object.</p>
newtype InstanceCredentials = InstanceCredentials 
  { "UserName" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Secret" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeInstanceCredentials :: Newtype InstanceCredentials _
derive instance repGenericInstanceCredentials :: Generic InstanceCredentials _
instance showInstanceCredentials :: Show InstanceCredentials where
  show = genericShow
instance decodeInstanceCredentials :: Decode InstanceCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceCredentials :: Encode InstanceCredentials where
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


newtype InstanceStatus = InstanceStatus String
derive instance newtypeInstanceStatus :: Newtype InstanceStatus _
derive instance repGenericInstanceStatus :: Generic InstanceStatus _
instance showInstanceStatus :: Show InstanceStatus where
  show = genericShow
instance decodeInstanceStatus :: Decode InstanceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceStatus :: Encode InstanceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service encountered an unrecoverable internal failure while processing the request. Clients can retry such requests immediately or after a waiting period.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _
derive instance repGenericInternalServiceException :: Generic InternalServiceException _
instance showInternalServiceException :: Show InternalServiceException where
  show = genericShow
instance decodeInternalServiceException :: Decode InternalServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceException :: Encode InternalServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested operation would cause a conflict with the current state of a resource associated with the request and/or the fleet. Resolve the conflict before retrying.</p>
newtype InvalidFleetStatusException = InvalidFleetStatusException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeInvalidFleetStatusException :: Newtype InvalidFleetStatusException _
derive instance repGenericInvalidFleetStatusException :: Generic InvalidFleetStatusException _
instance showInvalidFleetStatusException :: Show InvalidFleetStatusException where
  show = genericShow
instance decodeInvalidFleetStatusException :: Decode InvalidFleetStatusException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFleetStatusException :: Encode InvalidFleetStatusException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested operation would cause a conflict with the current state of a resource associated with the request and/or the game instance. Resolve the conflict before retrying.</p>
newtype InvalidGameSessionStatusException = InvalidGameSessionStatusException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeInvalidGameSessionStatusException :: Newtype InvalidGameSessionStatusException _
derive instance repGenericInvalidGameSessionStatusException :: Generic InvalidGameSessionStatusException _
instance showInvalidGameSessionStatusException :: Show InvalidGameSessionStatusException where
  show = genericShow
instance decodeInvalidGameSessionStatusException :: Decode InvalidGameSessionStatusException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidGameSessionStatusException :: Encode InvalidGameSessionStatusException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more parameter values in the request are invalid. Correct the invalid parameter values before retrying.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A range of IP addresses and port settings that allow inbound traffic to connect to server processes on Amazon GameLift. Each game session hosted on a fleet is assigned a unique combination of IP address and port number, which must fall into the fleet's allowed ranges. This combination is included in the <a>GameSession</a> object. </p>
newtype IpPermission = IpPermission 
  { "FromPort" :: (PortNumber)
  , "ToPort" :: (PortNumber)
  , "IpRange" :: (NonBlankString)
  , "Protocol" :: (IpProtocol)
  }
derive instance newtypeIpPermission :: Newtype IpPermission _
derive instance repGenericIpPermission :: Generic IpPermission _
instance showIpPermission :: Show IpPermission where
  show = genericShow
instance decodeIpPermission :: Decode IpPermission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpPermission :: Encode IpPermission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpPermissionsList = IpPermissionsList (Array IpPermission)
derive instance newtypeIpPermissionsList :: Newtype IpPermissionsList _
derive instance repGenericIpPermissionsList :: Generic IpPermissionsList _
instance showIpPermissionsList :: Show IpPermissionsList where
  show = genericShow
instance decodeIpPermissionsList :: Decode IpPermissionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpPermissionsList :: Encode IpPermissionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpProtocol = IpProtocol String
derive instance newtypeIpProtocol :: Newtype IpProtocol _
derive instance repGenericIpProtocol :: Generic IpProtocol _
instance showIpProtocol :: Show IpProtocol where
  show = genericShow
instance decodeIpProtocol :: Decode IpProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpProtocol :: Encode IpProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LatencyMap = LatencyMap (StrMap.StrMap PositiveInteger)
derive instance newtypeLatencyMap :: Newtype LatencyMap _
derive instance repGenericLatencyMap :: Generic LatencyMap _
instance showLatencyMap :: Show LatencyMap where
  show = genericShow
instance decodeLatencyMap :: Decode LatencyMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLatencyMap :: Encode LatencyMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested operation would cause the resource to exceed the allowed service limit. Resolve the issue before retrying.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype ListAliasesInput = ListAliasesInput 
  { "RoutingStrategyType" :: NullOrUndefined.NullOrUndefined (RoutingStrategyType)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeListAliasesInput :: Newtype ListAliasesInput _
derive instance repGenericListAliasesInput :: Generic ListAliasesInput _
instance showListAliasesInput :: Show ListAliasesInput where
  show = genericShow
instance decodeListAliasesInput :: Decode ListAliasesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAliasesInput :: Encode ListAliasesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype ListAliasesOutput = ListAliasesOutput 
  { "Aliases" :: NullOrUndefined.NullOrUndefined (AliasList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeListAliasesOutput :: Newtype ListAliasesOutput _
derive instance repGenericListAliasesOutput :: Generic ListAliasesOutput _
instance showListAliasesOutput :: Show ListAliasesOutput where
  show = genericShow
instance decodeListAliasesOutput :: Decode ListAliasesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAliasesOutput :: Encode ListAliasesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype ListBuildsInput = ListBuildsInput 
  { "Status" :: NullOrUndefined.NullOrUndefined (BuildStatus)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeListBuildsInput :: Newtype ListBuildsInput _
derive instance repGenericListBuildsInput :: Generic ListBuildsInput _
instance showListBuildsInput :: Show ListBuildsInput where
  show = genericShow
instance decodeListBuildsInput :: Decode ListBuildsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBuildsInput :: Encode ListBuildsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype ListBuildsOutput = ListBuildsOutput 
  { "Builds" :: NullOrUndefined.NullOrUndefined (BuildList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeListBuildsOutput :: Newtype ListBuildsOutput _
derive instance repGenericListBuildsOutput :: Generic ListBuildsOutput _
instance showListBuildsOutput :: Show ListBuildsOutput where
  show = genericShow
instance decodeListBuildsOutput :: Decode ListBuildsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBuildsOutput :: Encode ListBuildsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype ListFleetsInput = ListFleetsInput 
  { "BuildId" :: NullOrUndefined.NullOrUndefined (BuildId)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeListFleetsInput :: Newtype ListFleetsInput _
derive instance repGenericListFleetsInput :: Generic ListFleetsInput _
instance showListFleetsInput :: Show ListFleetsInput where
  show = genericShow
instance decodeListFleetsInput :: Decode ListFleetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFleetsInput :: Encode ListFleetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype ListFleetsOutput = ListFleetsOutput 
  { "FleetIds" :: NullOrUndefined.NullOrUndefined (FleetIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeListFleetsOutput :: Newtype ListFleetsOutput _
derive instance repGenericListFleetsOutput :: Generic ListFleetsOutput _
instance showListFleetsOutput :: Show ListFleetsOutput where
  show = genericShow
instance decodeListFleetsOutput :: Decode ListFleetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFleetsOutput :: Encode ListFleetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a new player session that is created as a result of a successful FlexMatch match. A successful match automatically creates new player sessions for every player ID in the original matchmaking request. </p> <p>When players connect to the match's game session, they must include both player ID and player session ID in order to claim their assigned player slot.</p>
newtype MatchedPlayerSession = MatchedPlayerSession 
  { "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerSessionId" :: NullOrUndefined.NullOrUndefined (PlayerSessionId)
  }
derive instance newtypeMatchedPlayerSession :: Newtype MatchedPlayerSession _
derive instance repGenericMatchedPlayerSession :: Generic MatchedPlayerSession _
instance showMatchedPlayerSession :: Show MatchedPlayerSession where
  show = genericShow
instance decodeMatchedPlayerSession :: Decode MatchedPlayerSession where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchedPlayerSession :: Encode MatchedPlayerSession where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchedPlayerSessionList = MatchedPlayerSessionList (Array MatchedPlayerSession)
derive instance newtypeMatchedPlayerSessionList :: Newtype MatchedPlayerSessionList _
derive instance repGenericMatchedPlayerSessionList :: Generic MatchedPlayerSessionList _
instance showMatchedPlayerSessionList :: Show MatchedPlayerSessionList where
  show = genericShow
instance decodeMatchedPlayerSessionList :: Decode MatchedPlayerSessionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchedPlayerSessionList :: Encode MatchedPlayerSessionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakerData = MatchmakerData String
derive instance newtypeMatchmakerData :: Newtype MatchmakerData _
derive instance repGenericMatchmakerData :: Generic MatchmakerData _
instance showMatchmakerData :: Show MatchmakerData where
  show = genericShow
instance decodeMatchmakerData :: Decode MatchmakerData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakerData :: Encode MatchmakerData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingAcceptanceTimeoutInteger = MatchmakingAcceptanceTimeoutInteger Int
derive instance newtypeMatchmakingAcceptanceTimeoutInteger :: Newtype MatchmakingAcceptanceTimeoutInteger _
derive instance repGenericMatchmakingAcceptanceTimeoutInteger :: Generic MatchmakingAcceptanceTimeoutInteger _
instance showMatchmakingAcceptanceTimeoutInteger :: Show MatchmakingAcceptanceTimeoutInteger where
  show = genericShow
instance decodeMatchmakingAcceptanceTimeoutInteger :: Decode MatchmakingAcceptanceTimeoutInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingAcceptanceTimeoutInteger :: Encode MatchmakingAcceptanceTimeoutInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Guidelines for use with FlexMatch to match players into games. All matchmaking requests must specify a matchmaking configuration.</p>
newtype MatchmakingConfiguration = MatchmakingConfiguration 
  { "Name" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionQueueArns" :: NullOrUndefined.NullOrUndefined (QueueArnsList)
  , "RequestTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (MatchmakingRequestTimeoutInteger)
  , "AcceptanceTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (MatchmakingAcceptanceTimeoutInteger)
  , "AcceptanceRequired" :: NullOrUndefined.NullOrUndefined (BooleanModel)
  , "RuleSetName" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "NotificationTarget" :: NullOrUndefined.NullOrUndefined (SnsArnStringModel)
  , "AdditionalPlayerCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "CustomEventData" :: NullOrUndefined.NullOrUndefined (CustomEventData)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  }
derive instance newtypeMatchmakingConfiguration :: Newtype MatchmakingConfiguration _
derive instance repGenericMatchmakingConfiguration :: Generic MatchmakingConfiguration _
instance showMatchmakingConfiguration :: Show MatchmakingConfiguration where
  show = genericShow
instance decodeMatchmakingConfiguration :: Decode MatchmakingConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingConfiguration :: Encode MatchmakingConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingConfigurationList = MatchmakingConfigurationList (Array MatchmakingConfiguration)
derive instance newtypeMatchmakingConfigurationList :: Newtype MatchmakingConfigurationList _
derive instance repGenericMatchmakingConfigurationList :: Generic MatchmakingConfigurationList _
instance showMatchmakingConfigurationList :: Show MatchmakingConfigurationList where
  show = genericShow
instance decodeMatchmakingConfigurationList :: Decode MatchmakingConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingConfigurationList :: Encode MatchmakingConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingConfigurationStatus = MatchmakingConfigurationStatus String
derive instance newtypeMatchmakingConfigurationStatus :: Newtype MatchmakingConfigurationStatus _
derive instance repGenericMatchmakingConfigurationStatus :: Generic MatchmakingConfigurationStatus _
instance showMatchmakingConfigurationStatus :: Show MatchmakingConfigurationStatus where
  show = genericShow
instance decodeMatchmakingConfigurationStatus :: Decode MatchmakingConfigurationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingConfigurationStatus :: Encode MatchmakingConfigurationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingIdList = MatchmakingIdList (Array MatchmakingIdStringModel)
derive instance newtypeMatchmakingIdList :: Newtype MatchmakingIdList _
derive instance repGenericMatchmakingIdList :: Generic MatchmakingIdList _
instance showMatchmakingIdList :: Show MatchmakingIdList where
  show = genericShow
instance decodeMatchmakingIdList :: Decode MatchmakingIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingIdList :: Encode MatchmakingIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingIdStringModel = MatchmakingIdStringModel String
derive instance newtypeMatchmakingIdStringModel :: Newtype MatchmakingIdStringModel _
derive instance repGenericMatchmakingIdStringModel :: Generic MatchmakingIdStringModel _
instance showMatchmakingIdStringModel :: Show MatchmakingIdStringModel where
  show = genericShow
instance decodeMatchmakingIdStringModel :: Decode MatchmakingIdStringModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingIdStringModel :: Encode MatchmakingIdStringModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingRequestTimeoutInteger = MatchmakingRequestTimeoutInteger Int
derive instance newtypeMatchmakingRequestTimeoutInteger :: Newtype MatchmakingRequestTimeoutInteger _
derive instance repGenericMatchmakingRequestTimeoutInteger :: Generic MatchmakingRequestTimeoutInteger _
instance showMatchmakingRequestTimeoutInteger :: Show MatchmakingRequestTimeoutInteger where
  show = genericShow
instance decodeMatchmakingRequestTimeoutInteger :: Decode MatchmakingRequestTimeoutInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingRequestTimeoutInteger :: Encode MatchmakingRequestTimeoutInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Set of rule statements, used with FlexMatch, that determine how to build a certain kind of player match. Each rule set describes a type of group to be created and defines the parameters for acceptable player matches. Rule sets are used in <a>MatchmakingConfiguration</a> objects.</p> <p>A rule set may define the following elements for a match. For detailed information and examples showing how to construct a rule set, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-rulesets.html">Build a FlexMatch Rule Set</a>. </p> <ul> <li> <p>Teams -- Required. A rule set must define one or multiple teams for the match and set minimum and maximum team sizes. For example, a rule set might describe a 4x4 match that requires all eight slots to be filled. </p> </li> <li> <p>Player attributes -- Optional. These attributes specify a set of player characteristics to evaluate when looking for a match. Matchmaking requests that use a rule set with player attributes must provide the corresponding attribute values. For example, an attribute might specify a player's skill or level.</p> </li> <li> <p>Rules -- Optional. Rules define how to evaluate potential players for a match based on player attributes. A rule might specify minimum requirements for individual players, teams, or entire matches. For example, a rule might require each player to meet a certain skill level, each team to have at least one player in a certain role, or the match to have a minimum average skill level. or may describe an entire group--such as all teams must be evenly matched or have at least one player in a certain role. </p> </li> <li> <p>Expansions -- Optional. Expansions allow you to relax the rules after a period of time when no acceptable matches are found. This feature lets you balance getting players into games in a reasonable amount of time instead of making them wait indefinitely for the best possible match. For example, you might use an expansion to increase the maximum skill variance between players after 30 seconds.</p> </li> </ul>
newtype MatchmakingRuleSet = MatchmakingRuleSet 
  { "RuleSetName" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "RuleSetBody" :: (RuleSetBody)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeMatchmakingRuleSet :: Newtype MatchmakingRuleSet _
derive instance repGenericMatchmakingRuleSet :: Generic MatchmakingRuleSet _
instance showMatchmakingRuleSet :: Show MatchmakingRuleSet where
  show = genericShow
instance decodeMatchmakingRuleSet :: Decode MatchmakingRuleSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingRuleSet :: Encode MatchmakingRuleSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingRuleSetList = MatchmakingRuleSetList (Array MatchmakingRuleSet)
derive instance newtypeMatchmakingRuleSetList :: Newtype MatchmakingRuleSetList _
derive instance repGenericMatchmakingRuleSetList :: Generic MatchmakingRuleSetList _
instance showMatchmakingRuleSetList :: Show MatchmakingRuleSetList where
  show = genericShow
instance decodeMatchmakingRuleSetList :: Decode MatchmakingRuleSetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingRuleSetList :: Encode MatchmakingRuleSetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingRuleSetNameList = MatchmakingRuleSetNameList (Array MatchmakingIdStringModel)
derive instance newtypeMatchmakingRuleSetNameList :: Newtype MatchmakingRuleSetNameList _
derive instance repGenericMatchmakingRuleSetNameList :: Generic MatchmakingRuleSetNameList _
instance showMatchmakingRuleSetNameList :: Show MatchmakingRuleSetNameList where
  show = genericShow
instance decodeMatchmakingRuleSetNameList :: Decode MatchmakingRuleSetNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingRuleSetNameList :: Encode MatchmakingRuleSetNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Ticket generated to track the progress of a matchmaking request. Each ticket is uniquely identified by a ticket ID, supplied by the requester, when creating a matchmaking request with <a>StartMatchmaking</a>. Tickets can be retrieved by calling <a>DescribeMatchmaking</a> with the ticket ID.</p>
newtype MatchmakingTicket = MatchmakingTicket 
  { "TicketId" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "ConfigurationName" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "Status" :: NullOrUndefined.NullOrUndefined (MatchmakingConfigurationStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (StringModel)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StringModel)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Players" :: NullOrUndefined.NullOrUndefined (PlayerList)
  , "GameSessionConnectionInfo" :: NullOrUndefined.NullOrUndefined (GameSessionConnectionInfo)
  , "EstimatedWaitTime" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeMatchmakingTicket :: Newtype MatchmakingTicket _
derive instance repGenericMatchmakingTicket :: Generic MatchmakingTicket _
instance showMatchmakingTicket :: Show MatchmakingTicket where
  show = genericShow
instance decodeMatchmakingTicket :: Decode MatchmakingTicket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingTicket :: Encode MatchmakingTicket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchmakingTicketList = MatchmakingTicketList (Array MatchmakingTicket)
derive instance newtypeMatchmakingTicketList :: Newtype MatchmakingTicketList _
derive instance repGenericMatchmakingTicketList :: Generic MatchmakingTicketList _
instance showMatchmakingTicketList :: Show MatchmakingTicketList where
  show = genericShow
instance decodeMatchmakingTicketList :: Decode MatchmakingTicketList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchmakingTicketList :: Encode MatchmakingTicketList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxConcurrentGameSessionActivations = MaxConcurrentGameSessionActivations Int
derive instance newtypeMaxConcurrentGameSessionActivations :: Newtype MaxConcurrentGameSessionActivations _
derive instance repGenericMaxConcurrentGameSessionActivations :: Generic MaxConcurrentGameSessionActivations _
instance showMaxConcurrentGameSessionActivations :: Show MaxConcurrentGameSessionActivations where
  show = genericShow
instance decodeMaxConcurrentGameSessionActivations :: Decode MaxConcurrentGameSessionActivations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxConcurrentGameSessionActivations :: Encode MaxConcurrentGameSessionActivations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricGroup = MetricGroup String
derive instance newtypeMetricGroup :: Newtype MetricGroup _
derive instance repGenericMetricGroup :: Generic MetricGroup _
instance showMetricGroup :: Show MetricGroup where
  show = genericShow
instance decodeMetricGroup :: Decode MetricGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricGroup :: Encode MetricGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricGroupList = MetricGroupList (Array MetricGroup)
derive instance newtypeMetricGroupList :: Newtype MetricGroupList _
derive instance repGenericMetricGroupList :: Generic MetricGroupList _
instance showMetricGroupList :: Show MetricGroupList where
  show = genericShow
instance decodeMetricGroupList :: Decode MetricGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricGroupList :: Encode MetricGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _
derive instance repGenericMetricName :: Generic MetricName _
instance showMetricName :: Show MetricName where
  show = genericShow
instance decodeMetricName :: Decode MetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricName :: Encode MetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonBlankAndLengthConstraintString = NonBlankAndLengthConstraintString String
derive instance newtypeNonBlankAndLengthConstraintString :: Newtype NonBlankAndLengthConstraintString _
derive instance repGenericNonBlankAndLengthConstraintString :: Generic NonBlankAndLengthConstraintString _
instance showNonBlankAndLengthConstraintString :: Show NonBlankAndLengthConstraintString where
  show = genericShow
instance decodeNonBlankAndLengthConstraintString :: Decode NonBlankAndLengthConstraintString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonBlankAndLengthConstraintString :: Encode NonBlankAndLengthConstraintString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonBlankString = NonBlankString String
derive instance newtypeNonBlankString :: Newtype NonBlankString _
derive instance repGenericNonBlankString :: Generic NonBlankString _
instance showNonBlankString :: Show NonBlankString where
  show = genericShow
instance decodeNonBlankString :: Decode NonBlankString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonBlankString :: Encode NonBlankString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _
derive instance repGenericNonEmptyString :: Generic NonEmptyString _
instance showNonEmptyString :: Show NonEmptyString where
  show = genericShow
instance decodeNonEmptyString :: Decode NonEmptyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyString :: Encode NonEmptyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonZeroAndMaxString = NonZeroAndMaxString String
derive instance newtypeNonZeroAndMaxString :: Newtype NonZeroAndMaxString _
derive instance repGenericNonZeroAndMaxString :: Generic NonZeroAndMaxString _
instance showNonZeroAndMaxString :: Show NonZeroAndMaxString where
  show = genericShow
instance decodeNonZeroAndMaxString :: Decode NonZeroAndMaxString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonZeroAndMaxString :: Encode NonZeroAndMaxString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A service resource associated with the request could not be found. Clients should not retry such requests.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperatingSystem = OperatingSystem String
derive instance newtypeOperatingSystem :: Newtype OperatingSystem _
derive instance repGenericOperatingSystem :: Generic OperatingSystem _
instance showOperatingSystem :: Show OperatingSystem where
  show = genericShow
instance decodeOperatingSystem :: Decode OperatingSystem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperatingSystem :: Encode OperatingSystem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a player session that was created as part of a <a>StartGameSessionPlacement</a> request. This object contains only the player ID and player session ID. To retrieve full details on a player session, call <a>DescribePlayerSessions</a> with the player session ID.</p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
newtype PlacedPlayerSession = PlacedPlayerSession 
  { "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerSessionId" :: NullOrUndefined.NullOrUndefined (PlayerSessionId)
  }
derive instance newtypePlacedPlayerSession :: Newtype PlacedPlayerSession _
derive instance repGenericPlacedPlayerSession :: Generic PlacedPlayerSession _
instance showPlacedPlayerSession :: Show PlacedPlayerSession where
  show = genericShow
instance decodePlacedPlayerSession :: Decode PlacedPlayerSession where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacedPlayerSession :: Encode PlacedPlayerSession where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlacedPlayerSessionList = PlacedPlayerSessionList (Array PlacedPlayerSession)
derive instance newtypePlacedPlayerSessionList :: Newtype PlacedPlayerSessionList _
derive instance repGenericPlacedPlayerSessionList :: Generic PlacedPlayerSessionList _
instance showPlacedPlayerSessionList :: Show PlacedPlayerSessionList where
  show = genericShow
instance decodePlacedPlayerSessionList :: Decode PlacedPlayerSessionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacedPlayerSessionList :: Encode PlacedPlayerSessionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a player in matchmaking. When starting a matchmaking request, a player has a player ID, attributes, and may have latency data. Team information is added after a match has been successfully completed.</p>
newtype Player = Player 
  { "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerAttributes" :: NullOrUndefined.NullOrUndefined (PlayerAttributeMap)
  , "Team" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "LatencyInMs" :: NullOrUndefined.NullOrUndefined (LatencyMap)
  }
derive instance newtypePlayer :: Newtype Player _
derive instance repGenericPlayer :: Generic Player _
instance showPlayer :: Show Player where
  show = genericShow
instance decodePlayer :: Decode Player where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayer :: Encode Player where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerAttributeMap = PlayerAttributeMap (StrMap.StrMap AttributeValue)
derive instance newtypePlayerAttributeMap :: Newtype PlayerAttributeMap _
derive instance repGenericPlayerAttributeMap :: Generic PlayerAttributeMap _
instance showPlayerAttributeMap :: Show PlayerAttributeMap where
  show = genericShow
instance decodePlayerAttributeMap :: Decode PlayerAttributeMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerAttributeMap :: Encode PlayerAttributeMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerData = PlayerData String
derive instance newtypePlayerData :: Newtype PlayerData _
derive instance repGenericPlayerData :: Generic PlayerData _
instance showPlayerData :: Show PlayerData where
  show = genericShow
instance decodePlayerData :: Decode PlayerData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerData :: Encode PlayerData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerDataMap = PlayerDataMap (StrMap.StrMap PlayerData)
derive instance newtypePlayerDataMap :: Newtype PlayerDataMap _
derive instance repGenericPlayerDataMap :: Generic PlayerDataMap _
instance showPlayerDataMap :: Show PlayerDataMap where
  show = genericShow
instance decodePlayerDataMap :: Decode PlayerDataMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerDataMap :: Encode PlayerDataMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerIdList = PlayerIdList (Array NonZeroAndMaxString)
derive instance newtypePlayerIdList :: Newtype PlayerIdList _
derive instance repGenericPlayerIdList :: Generic PlayerIdList _
instance showPlayerIdList :: Show PlayerIdList where
  show = genericShow
instance decodePlayerIdList :: Decode PlayerIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerIdList :: Encode PlayerIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Regional latency information for a player, used when requesting a new game session with <a>StartGameSessionPlacement</a>. This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified region. The relative difference between a player's latency values for multiple regions are used to determine which fleets are best suited to place a new game session for the player. </p>
newtype PlayerLatency = PlayerLatency 
  { "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "RegionIdentifier" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "LatencyInMilliseconds" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePlayerLatency :: Newtype PlayerLatency _
derive instance repGenericPlayerLatency :: Generic PlayerLatency _
instance showPlayerLatency :: Show PlayerLatency where
  show = genericShow
instance decodePlayerLatency :: Decode PlayerLatency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerLatency :: Encode PlayerLatency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerLatencyList = PlayerLatencyList (Array PlayerLatency)
derive instance newtypePlayerLatencyList :: Newtype PlayerLatencyList _
derive instance repGenericPlayerLatencyList :: Generic PlayerLatencyList _
instance showPlayerLatencyList :: Show PlayerLatencyList where
  show = genericShow
instance decodePlayerLatencyList :: Decode PlayerLatencyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerLatencyList :: Encode PlayerLatencyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Queue setting that determines the highest latency allowed for individual players when placing a game session. When a latency policy is in force, a game session cannot be placed at any destination in a region where a player is reporting latency higher than the cap. Latency policies are only enforced when the placement request contains player latency information.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>
newtype PlayerLatencyPolicy = PlayerLatencyPolicy 
  { "MaximumIndividualPlayerLatencyMilliseconds" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "PolicyDurationSeconds" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypePlayerLatencyPolicy :: Newtype PlayerLatencyPolicy _
derive instance repGenericPlayerLatencyPolicy :: Generic PlayerLatencyPolicy _
instance showPlayerLatencyPolicy :: Show PlayerLatencyPolicy where
  show = genericShow
instance decodePlayerLatencyPolicy :: Decode PlayerLatencyPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerLatencyPolicy :: Encode PlayerLatencyPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerLatencyPolicyList = PlayerLatencyPolicyList (Array PlayerLatencyPolicy)
derive instance newtypePlayerLatencyPolicyList :: Newtype PlayerLatencyPolicyList _
derive instance repGenericPlayerLatencyPolicyList :: Generic PlayerLatencyPolicyList _
instance showPlayerLatencyPolicyList :: Show PlayerLatencyPolicyList where
  show = genericShow
instance decodePlayerLatencyPolicyList :: Decode PlayerLatencyPolicyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerLatencyPolicyList :: Encode PlayerLatencyPolicyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerList = PlayerList (Array Player)
derive instance newtypePlayerList :: Newtype PlayerList _
derive instance repGenericPlayerList :: Generic PlayerList _
instance showPlayerList :: Show PlayerList where
  show = genericShow
instance decodePlayerList :: Decode PlayerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerList :: Encode PlayerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Properties describing a player session. Player session objects are created either by creating a player session for a specific game session, or as part of a game session placement. A player session represents either a player reservation for a game session (status <code>RESERVED</code>) or actual player activity in a game session (status <code>ACTIVE</code>). A player session object (including player data) is automatically passed to a game session when the player connects to the game session and is validated.</p> <p>When a player disconnects, the player session status changes to <code>COMPLETED</code>. Once the session ends, the player session object is retained for 30 days and then removed.</p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>
newtype PlayerSession = PlayerSession 
  { "PlayerSessionId" :: NullOrUndefined.NullOrUndefined (PlayerSessionId)
  , "PlayerId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "TerminationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Status" :: NullOrUndefined.NullOrUndefined (PlayerSessionStatus)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "Port" :: NullOrUndefined.NullOrUndefined (PortNumber)
  , "PlayerData" :: NullOrUndefined.NullOrUndefined (PlayerData)
  }
derive instance newtypePlayerSession :: Newtype PlayerSession _
derive instance repGenericPlayerSession :: Generic PlayerSession _
instance showPlayerSession :: Show PlayerSession where
  show = genericShow
instance decodePlayerSession :: Decode PlayerSession where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerSession :: Encode PlayerSession where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerSessionCreationPolicy = PlayerSessionCreationPolicy String
derive instance newtypePlayerSessionCreationPolicy :: Newtype PlayerSessionCreationPolicy _
derive instance repGenericPlayerSessionCreationPolicy :: Generic PlayerSessionCreationPolicy _
instance showPlayerSessionCreationPolicy :: Show PlayerSessionCreationPolicy where
  show = genericShow
instance decodePlayerSessionCreationPolicy :: Decode PlayerSessionCreationPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerSessionCreationPolicy :: Encode PlayerSessionCreationPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerSessionId = PlayerSessionId String
derive instance newtypePlayerSessionId :: Newtype PlayerSessionId _
derive instance repGenericPlayerSessionId :: Generic PlayerSessionId _
instance showPlayerSessionId :: Show PlayerSessionId where
  show = genericShow
instance decodePlayerSessionId :: Decode PlayerSessionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerSessionId :: Encode PlayerSessionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerSessionList = PlayerSessionList (Array PlayerSession)
derive instance newtypePlayerSessionList :: Newtype PlayerSessionList _
derive instance repGenericPlayerSessionList :: Generic PlayerSessionList _
instance showPlayerSessionList :: Show PlayerSessionList where
  show = genericShow
instance decodePlayerSessionList :: Decode PlayerSessionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerSessionList :: Encode PlayerSessionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayerSessionStatus = PlayerSessionStatus String
derive instance newtypePlayerSessionStatus :: Newtype PlayerSessionStatus _
derive instance repGenericPlayerSessionStatus :: Generic PlayerSessionStatus _
instance showPlayerSessionStatus :: Show PlayerSessionStatus where
  show = genericShow
instance decodePlayerSessionStatus :: Decode PlayerSessionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayerSessionStatus :: Encode PlayerSessionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortNumber = PortNumber Int
derive instance newtypePortNumber :: Newtype PortNumber _
derive instance repGenericPortNumber :: Generic PortNumber _
instance showPortNumber :: Show PortNumber where
  show = genericShow
instance decodePortNumber :: Decode PortNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortNumber :: Encode PortNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveInteger = PositiveInteger Int
derive instance newtypePositiveInteger :: Newtype PositiveInteger _
derive instance repGenericPositiveInteger :: Generic PositiveInteger _
instance showPositiveInteger :: Show PositiveInteger where
  show = genericShow
instance decodePositiveInteger :: Decode PositiveInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveInteger :: Encode PositiveInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveLong = PositiveLong Number
derive instance newtypePositiveLong :: Newtype PositiveLong _
derive instance repGenericPositiveLong :: Generic PositiveLong _
instance showPositiveLong :: Show PositiveLong where
  show = genericShow
instance decodePositiveLong :: Decode PositiveLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveLong :: Encode PositiveLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProtectionPolicy = ProtectionPolicy String
derive instance newtypeProtectionPolicy :: Newtype ProtectionPolicy _
derive instance repGenericProtectionPolicy :: Generic ProtectionPolicy _
instance showProtectionPolicy :: Show ProtectionPolicy where
  show = genericShow
instance decodeProtectionPolicy :: Decode ProtectionPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProtectionPolicy :: Encode ProtectionPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype PutScalingPolicyInput = PutScalingPolicyInput 
  { "Name" :: (NonZeroAndMaxString)
  , "FleetId" :: (FleetId)
  , "ScalingAdjustment" :: (Int)
  , "ScalingAdjustmentType" :: (ScalingAdjustmentType)
  , "Threshold" :: (Number)
  , "ComparisonOperator" :: (ComparisonOperatorType)
  , "EvaluationPeriods" :: (PositiveInteger)
  , "MetricName" :: (MetricName)
  }
derive instance newtypePutScalingPolicyInput :: Newtype PutScalingPolicyInput _
derive instance repGenericPutScalingPolicyInput :: Generic PutScalingPolicyInput _
instance showPutScalingPolicyInput :: Show PutScalingPolicyInput where
  show = genericShow
instance decodePutScalingPolicyInput :: Decode PutScalingPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutScalingPolicyInput :: Encode PutScalingPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype PutScalingPolicyOutput = PutScalingPolicyOutput 
  { "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypePutScalingPolicyOutput :: Newtype PutScalingPolicyOutput _
derive instance repGenericPutScalingPolicyOutput :: Generic PutScalingPolicyOutput _
instance showPutScalingPolicyOutput :: Show PutScalingPolicyOutput where
  show = genericShow
instance decodePutScalingPolicyOutput :: Decode PutScalingPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutScalingPolicyOutput :: Encode PutScalingPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueArnsList = QueueArnsList (Array ArnStringModel)
derive instance newtypeQueueArnsList :: Newtype QueueArnsList _
derive instance repGenericQueueArnsList :: Generic QueueArnsList _
instance showQueueArnsList :: Show QueueArnsList where
  show = genericShow
instance decodeQueueArnsList :: Decode QueueArnsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueArnsList :: Encode QueueArnsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype RequestUploadCredentialsInput = RequestUploadCredentialsInput 
  { "BuildId" :: (BuildId)
  }
derive instance newtypeRequestUploadCredentialsInput :: Newtype RequestUploadCredentialsInput _
derive instance repGenericRequestUploadCredentialsInput :: Generic RequestUploadCredentialsInput _
instance showRequestUploadCredentialsInput :: Show RequestUploadCredentialsInput where
  show = genericShow
instance decodeRequestUploadCredentialsInput :: Decode RequestUploadCredentialsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestUploadCredentialsInput :: Encode RequestUploadCredentialsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype RequestUploadCredentialsOutput = RequestUploadCredentialsOutput 
  { "UploadCredentials" :: NullOrUndefined.NullOrUndefined (AwsCredentials)
  , "StorageLocation" :: NullOrUndefined.NullOrUndefined (S3Location)
  }
derive instance newtypeRequestUploadCredentialsOutput :: Newtype RequestUploadCredentialsOutput _
derive instance repGenericRequestUploadCredentialsOutput :: Generic RequestUploadCredentialsOutput _
instance showRequestUploadCredentialsOutput :: Show RequestUploadCredentialsOutput where
  show = genericShow
instance decodeRequestUploadCredentialsOutput :: Decode RequestUploadCredentialsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestUploadCredentialsOutput :: Encode RequestUploadCredentialsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype ResolveAliasInput = ResolveAliasInput 
  { "AliasId" :: (AliasId)
  }
derive instance newtypeResolveAliasInput :: Newtype ResolveAliasInput _
derive instance repGenericResolveAliasInput :: Generic ResolveAliasInput _
instance showResolveAliasInput :: Show ResolveAliasInput where
  show = genericShow
instance decodeResolveAliasInput :: Decode ResolveAliasInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolveAliasInput :: Encode ResolveAliasInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype ResolveAliasOutput = ResolveAliasOutput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  }
derive instance newtypeResolveAliasOutput :: Newtype ResolveAliasOutput _
derive instance repGenericResolveAliasOutput :: Generic ResolveAliasOutput _
instance showResolveAliasOutput :: Show ResolveAliasOutput where
  show = genericShow
instance decodeResolveAliasOutput :: Decode ResolveAliasOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolveAliasOutput :: Encode ResolveAliasOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Policy that limits the number of game sessions a player can create on the same fleet. This optional policy gives game owners control over how players can consume available game server resources. A resource creation policy makes the following statement: "An individual player can create a maximum number of new game sessions within a specified time period".</p> <p>The policy is evaluated when a player tries to create a new game session. For example, with a policy of 10 new game sessions and a time period of 60 minutes, on receiving a <code>CreateGameSession</code> request, Amazon GameLift checks that the player (identified by <code>CreatorId</code>) has created fewer than 10 game sessions in the past 60 minutes.</p>
newtype ResourceCreationLimitPolicy = ResourceCreationLimitPolicy 
  { "NewGameSessionsPerCreator" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "PolicyPeriodInMinutes" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeResourceCreationLimitPolicy :: Newtype ResourceCreationLimitPolicy _
derive instance repGenericResourceCreationLimitPolicy :: Generic ResourceCreationLimitPolicy _
instance showResourceCreationLimitPolicy :: Show ResourceCreationLimitPolicy where
  show = genericShow
instance decodeResourceCreationLimitPolicy :: Decode ResourceCreationLimitPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceCreationLimitPolicy :: Encode ResourceCreationLimitPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Routing configuration for a fleet alias.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype RoutingStrategy = RoutingStrategy 
  { "Type" :: NullOrUndefined.NullOrUndefined (RoutingStrategyType)
  , "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "Message" :: NullOrUndefined.NullOrUndefined (FreeText)
  }
derive instance newtypeRoutingStrategy :: Newtype RoutingStrategy _
derive instance repGenericRoutingStrategy :: Generic RoutingStrategy _
instance showRoutingStrategy :: Show RoutingStrategy where
  show = genericShow
instance decodeRoutingStrategy :: Decode RoutingStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoutingStrategy :: Encode RoutingStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoutingStrategyType = RoutingStrategyType String
derive instance newtypeRoutingStrategyType :: Newtype RoutingStrategyType _
derive instance repGenericRoutingStrategyType :: Generic RoutingStrategyType _
instance showRoutingStrategyType :: Show RoutingStrategyType where
  show = genericShow
instance decodeRoutingStrategyType :: Decode RoutingStrategyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoutingStrategyType :: Encode RoutingStrategyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleSetBody = RuleSetBody String
derive instance newtypeRuleSetBody :: Newtype RuleSetBody _
derive instance repGenericRuleSetBody :: Generic RuleSetBody _
instance showRuleSetBody :: Show RuleSetBody where
  show = genericShow
instance decodeRuleSetBody :: Decode RuleSetBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleSetBody :: Encode RuleSetBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleSetLimit = RuleSetLimit Int
derive instance newtypeRuleSetLimit :: Newtype RuleSetLimit _
derive instance repGenericRuleSetLimit :: Generic RuleSetLimit _
instance showRuleSetLimit :: Show RuleSetLimit where
  show = genericShow
instance decodeRuleSetLimit :: Decode RuleSetLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleSetLimit :: Encode RuleSetLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of server process configurations that describe what processes to run on each instance in a fleet. All fleets must have a run-time configuration. Each instance in the fleet launches the server processes specified in the run-time configuration and launches new ones as existing processes end. Each instance regularly checks for an updated run-time configuration and follows the new instructions. </p> <p>The run-time configuration enables the instances in a fleet to run multiple processes simultaneously. Potential scenarios are as follows: (1) Run multiple processes of a single game server executable to maximize usage of your hosting resources. (2) Run one or more processes of different build executables, such as your game server executable and a related program, or two or more different versions of a game server. (3) Run multiple processes of a single game server but with different launch parameters, for example to run one process on each instance in debug mode.</p> <p>A Amazon GameLift instance is limited to 50 processes running simultaneously. A run-time configuration must specify fewer than this limit. To calculate the total number of processes specified in a run-time configuration, add the values of the <code>ConcurrentExecutions</code> parameter for each <code> <a>ServerProcess</a> </code> object in the run-time configuration.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype RuntimeConfiguration = RuntimeConfiguration 
  { "ServerProcesses" :: NullOrUndefined.NullOrUndefined (ServerProcessList)
  , "MaxConcurrentGameSessionActivations" :: NullOrUndefined.NullOrUndefined (MaxConcurrentGameSessionActivations)
  , "GameSessionActivationTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (GameSessionActivationTimeoutSeconds)
  }
derive instance newtypeRuntimeConfiguration :: Newtype RuntimeConfiguration _
derive instance repGenericRuntimeConfiguration :: Generic RuntimeConfiguration _
instance showRuntimeConfiguration :: Show RuntimeConfiguration where
  show = genericShow
instance decodeRuntimeConfiguration :: Decode RuntimeConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuntimeConfiguration :: Encode RuntimeConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Location in Amazon Simple Storage Service (Amazon S3) where build files can be stored for access by Amazon GameLift. This location is specified in a <a>CreateBuild</a> request. For more details, see the <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build">Create a Build with Files in Amazon S3</a>.</p>
newtype S3Location = S3Location 
  { "Bucket" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Key" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeS3Location :: Newtype S3Location _
derive instance repGenericS3Location :: Generic S3Location _
instance showS3Location :: Show S3Location where
  show = genericShow
instance decodeS3Location :: Decode S3Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Location :: Encode S3Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScalingAdjustmentType = ScalingAdjustmentType String
derive instance newtypeScalingAdjustmentType :: Newtype ScalingAdjustmentType _
derive instance repGenericScalingAdjustmentType :: Generic ScalingAdjustmentType _
instance showScalingAdjustmentType :: Show ScalingAdjustmentType where
  show = genericShow
instance decodeScalingAdjustmentType :: Decode ScalingAdjustmentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingAdjustmentType :: Encode ScalingAdjustmentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Rule that controls how a fleet is scaled. Scaling policies are uniquely identified by the combination of name and fleet ID.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>
newtype ScalingPolicy = ScalingPolicy 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Status" :: NullOrUndefined.NullOrUndefined (ScalingStatusType)
  , "ScalingAdjustment" :: NullOrUndefined.NullOrUndefined (Int)
  , "ScalingAdjustmentType" :: NullOrUndefined.NullOrUndefined (ScalingAdjustmentType)
  , "ComparisonOperator" :: NullOrUndefined.NullOrUndefined (ComparisonOperatorType)
  , "Threshold" :: NullOrUndefined.NullOrUndefined (Number)
  , "EvaluationPeriods" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  }
derive instance newtypeScalingPolicy :: Newtype ScalingPolicy _
derive instance repGenericScalingPolicy :: Generic ScalingPolicy _
instance showScalingPolicy :: Show ScalingPolicy where
  show = genericShow
instance decodeScalingPolicy :: Decode ScalingPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingPolicy :: Encode ScalingPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScalingPolicyList = ScalingPolicyList (Array ScalingPolicy)
derive instance newtypeScalingPolicyList :: Newtype ScalingPolicyList _
derive instance repGenericScalingPolicyList :: Generic ScalingPolicyList _
instance showScalingPolicyList :: Show ScalingPolicyList where
  show = genericShow
instance decodeScalingPolicyList :: Decode ScalingPolicyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingPolicyList :: Encode ScalingPolicyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScalingStatusType = ScalingStatusType String
derive instance newtypeScalingStatusType :: Newtype ScalingStatusType _
derive instance repGenericScalingStatusType :: Generic ScalingStatusType _
instance showScalingStatusType :: Show ScalingStatusType where
  show = genericShow
instance decodeScalingStatusType :: Decode ScalingStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingStatusType :: Encode ScalingStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype SearchGameSessionsInput = SearchGameSessionsInput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "AliasId" :: NullOrUndefined.NullOrUndefined (AliasId)
  , "FilterExpression" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "SortExpression" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveInteger)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeSearchGameSessionsInput :: Newtype SearchGameSessionsInput _
derive instance repGenericSearchGameSessionsInput :: Generic SearchGameSessionsInput _
instance showSearchGameSessionsInput :: Show SearchGameSessionsInput where
  show = genericShow
instance decodeSearchGameSessionsInput :: Decode SearchGameSessionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchGameSessionsInput :: Encode SearchGameSessionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype SearchGameSessionsOutput = SearchGameSessionsOutput 
  { "GameSessions" :: NullOrUndefined.NullOrUndefined (GameSessionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeSearchGameSessionsOutput :: Newtype SearchGameSessionsOutput _
derive instance repGenericSearchGameSessionsOutput :: Generic SearchGameSessionsOutput _
instance showSearchGameSessionsOutput :: Show SearchGameSessionsOutput where
  show = genericShow
instance decodeSearchGameSessionsOutput :: Decode SearchGameSessionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchGameSessionsOutput :: Encode SearchGameSessionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of instructions for launching server processes on each instance in a fleet. Each instruction set identifies the location of the server executable, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's <code> <a>RuntimeConfiguration</a> </code>.</p>
newtype ServerProcess = ServerProcess 
  { "LaunchPath" :: (NonZeroAndMaxString)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "ConcurrentExecutions" :: (PositiveInteger)
  }
derive instance newtypeServerProcess :: Newtype ServerProcess _
derive instance repGenericServerProcess :: Generic ServerProcess _
instance showServerProcess :: Show ServerProcess where
  show = genericShow
instance decodeServerProcess :: Decode ServerProcess where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerProcess :: Encode ServerProcess where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServerProcessList = ServerProcessList (Array ServerProcess)
derive instance newtypeServerProcessList :: Newtype ServerProcessList _
derive instance repGenericServerProcessList :: Generic ServerProcessList _
instance showServerProcessList :: Show ServerProcessList where
  show = genericShow
instance decodeServerProcessList :: Decode ServerProcessList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerProcessList :: Encode ServerProcessList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnsArnStringModel = SnsArnStringModel String
derive instance newtypeSnsArnStringModel :: Newtype SnsArnStringModel _
derive instance repGenericSnsArnStringModel :: Generic SnsArnStringModel _
instance showSnsArnStringModel :: Show SnsArnStringModel where
  show = genericShow
instance decodeSnsArnStringModel :: Decode SnsArnStringModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnsArnStringModel :: Encode SnsArnStringModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype StartGameSessionPlacementInput = StartGameSessionPlacementInput 
  { "PlacementId" :: (IdStringModel)
  , "GameSessionQueueName" :: (GameSessionQueueName)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "MaximumPlayerSessionCount" :: (WholeNumber)
  , "GameSessionName" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerLatencies" :: NullOrUndefined.NullOrUndefined (PlayerLatencyList)
  , "DesiredPlayerSessions" :: NullOrUndefined.NullOrUndefined (DesiredPlayerSessionList)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  }
derive instance newtypeStartGameSessionPlacementInput :: Newtype StartGameSessionPlacementInput _
derive instance repGenericStartGameSessionPlacementInput :: Generic StartGameSessionPlacementInput _
instance showStartGameSessionPlacementInput :: Show StartGameSessionPlacementInput where
  show = genericShow
instance decodeStartGameSessionPlacementInput :: Decode StartGameSessionPlacementInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartGameSessionPlacementInput :: Encode StartGameSessionPlacementInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype StartGameSessionPlacementOutput = StartGameSessionPlacementOutput 
  { "GameSessionPlacement" :: NullOrUndefined.NullOrUndefined (GameSessionPlacement)
  }
derive instance newtypeStartGameSessionPlacementOutput :: Newtype StartGameSessionPlacementOutput _
derive instance repGenericStartGameSessionPlacementOutput :: Generic StartGameSessionPlacementOutput _
instance showStartGameSessionPlacementOutput :: Show StartGameSessionPlacementOutput where
  show = genericShow
instance decodeStartGameSessionPlacementOutput :: Decode StartGameSessionPlacementOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartGameSessionPlacementOutput :: Encode StartGameSessionPlacementOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype StartMatchBackfillInput = StartMatchBackfillInput 
  { "TicketId" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "ConfigurationName" :: (MatchmakingIdStringModel)
  , "GameSessionArn" :: (ArnStringModel)
  , "Players" :: (PlayerList)
  }
derive instance newtypeStartMatchBackfillInput :: Newtype StartMatchBackfillInput _
derive instance repGenericStartMatchBackfillInput :: Generic StartMatchBackfillInput _
instance showStartMatchBackfillInput :: Show StartMatchBackfillInput where
  show = genericShow
instance decodeStartMatchBackfillInput :: Decode StartMatchBackfillInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMatchBackfillInput :: Encode StartMatchBackfillInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype StartMatchBackfillOutput = StartMatchBackfillOutput 
  { "MatchmakingTicket" :: NullOrUndefined.NullOrUndefined (MatchmakingTicket)
  }
derive instance newtypeStartMatchBackfillOutput :: Newtype StartMatchBackfillOutput _
derive instance repGenericStartMatchBackfillOutput :: Generic StartMatchBackfillOutput _
instance showStartMatchBackfillOutput :: Show StartMatchBackfillOutput where
  show = genericShow
instance decodeStartMatchBackfillOutput :: Decode StartMatchBackfillOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMatchBackfillOutput :: Encode StartMatchBackfillOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype StartMatchmakingInput = StartMatchmakingInput 
  { "TicketId" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "ConfigurationName" :: (MatchmakingIdStringModel)
  , "Players" :: (PlayerList)
  }
derive instance newtypeStartMatchmakingInput :: Newtype StartMatchmakingInput _
derive instance repGenericStartMatchmakingInput :: Generic StartMatchmakingInput _
instance showStartMatchmakingInput :: Show StartMatchmakingInput where
  show = genericShow
instance decodeStartMatchmakingInput :: Decode StartMatchmakingInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMatchmakingInput :: Encode StartMatchmakingInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype StartMatchmakingOutput = StartMatchmakingOutput 
  { "MatchmakingTicket" :: NullOrUndefined.NullOrUndefined (MatchmakingTicket)
  }
derive instance newtypeStartMatchmakingOutput :: Newtype StartMatchmakingOutput _
derive instance repGenericStartMatchmakingOutput :: Generic StartMatchmakingOutput _
instance showStartMatchmakingOutput :: Show StartMatchmakingOutput where
  show = genericShow
instance decodeStartMatchmakingOutput :: Decode StartMatchmakingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMatchmakingOutput :: Encode StartMatchmakingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype StopGameSessionPlacementInput = StopGameSessionPlacementInput 
  { "PlacementId" :: (IdStringModel)
  }
derive instance newtypeStopGameSessionPlacementInput :: Newtype StopGameSessionPlacementInput _
derive instance repGenericStopGameSessionPlacementInput :: Generic StopGameSessionPlacementInput _
instance showStopGameSessionPlacementInput :: Show StopGameSessionPlacementInput where
  show = genericShow
instance decodeStopGameSessionPlacementInput :: Decode StopGameSessionPlacementInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopGameSessionPlacementInput :: Encode StopGameSessionPlacementInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype StopGameSessionPlacementOutput = StopGameSessionPlacementOutput 
  { "GameSessionPlacement" :: NullOrUndefined.NullOrUndefined (GameSessionPlacement)
  }
derive instance newtypeStopGameSessionPlacementOutput :: Newtype StopGameSessionPlacementOutput _
derive instance repGenericStopGameSessionPlacementOutput :: Generic StopGameSessionPlacementOutput _
instance showStopGameSessionPlacementOutput :: Show StopGameSessionPlacementOutput where
  show = genericShow
instance decodeStopGameSessionPlacementOutput :: Decode StopGameSessionPlacementOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopGameSessionPlacementOutput :: Encode StopGameSessionPlacementOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype StopMatchmakingInput = StopMatchmakingInput 
  { "TicketId" :: (MatchmakingIdStringModel)
  }
derive instance newtypeStopMatchmakingInput :: Newtype StopMatchmakingInput _
derive instance repGenericStopMatchmakingInput :: Generic StopMatchmakingInput _
instance showStopMatchmakingInput :: Show StopMatchmakingInput where
  show = genericShow
instance decodeStopMatchmakingInput :: Decode StopMatchmakingInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopMatchmakingInput :: Encode StopMatchmakingInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopMatchmakingOutput = StopMatchmakingOutput Types.NoArguments
derive instance newtypeStopMatchmakingOutput :: Newtype StopMatchmakingOutput _
derive instance repGenericStopMatchmakingOutput :: Generic StopMatchmakingOutput _
instance showStopMatchmakingOutput :: Show StopMatchmakingOutput where
  show = genericShow
instance decodeStopMatchmakingOutput :: Decode StopMatchmakingOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopMatchmakingOutput :: Encode StopMatchmakingOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringDoubleMap = StringDoubleMap (StrMap.StrMap DoubleObject)
derive instance newtypeStringDoubleMap :: Newtype StringDoubleMap _
derive instance repGenericStringDoubleMap :: Generic StringDoubleMap _
instance showStringDoubleMap :: Show StringDoubleMap where
  show = genericShow
instance decodeStringDoubleMap :: Decode StringDoubleMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringDoubleMap :: Encode StringDoubleMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array NonZeroAndMaxString)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringModel = StringModel String
derive instance newtypeStringModel :: Newtype StringModel _
derive instance repGenericStringModel :: Generic StringModel _
instance showStringModel :: Show StringModel where
  show = genericShow
instance decodeStringModel :: Decode StringModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringModel :: Encode StringModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service is unable to resolve the routing for a particular alias because it has a terminal <a>RoutingStrategy</a> associated with it. The message returned in this exception is the message defined in the routing strategy itself. Such requests should only be retried if the routing strategy for the specified alias is modified. </p>
newtype TerminalRoutingStrategyException = TerminalRoutingStrategyException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeTerminalRoutingStrategyException :: Newtype TerminalRoutingStrategyException _
derive instance repGenericTerminalRoutingStrategyException :: Generic TerminalRoutingStrategyException _
instance showTerminalRoutingStrategyException :: Show TerminalRoutingStrategyException where
  show = genericShow
instance decodeTerminalRoutingStrategyException :: Decode TerminalRoutingStrategyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminalRoutingStrategyException :: Encode TerminalRoutingStrategyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The client failed authentication. Clients should not retry such requests.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _
derive instance repGenericUnauthorizedException :: Generic UnauthorizedException _
instance showUnauthorizedException :: Show UnauthorizedException where
  show = genericShow
instance decodeUnauthorizedException :: Decode UnauthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedException :: Encode UnauthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested operation is not supported in the region specified.</p>
newtype UnsupportedRegionException = UnsupportedRegionException 
  { "Message" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeUnsupportedRegionException :: Newtype UnsupportedRegionException _
derive instance repGenericUnsupportedRegionException :: Generic UnsupportedRegionException _
instance showUnsupportedRegionException :: Show UnsupportedRegionException where
  show = genericShow
instance decodeUnsupportedRegionException :: Decode UnsupportedRegionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedRegionException :: Encode UnsupportedRegionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateAliasInput = UpdateAliasInput 
  { "AliasId" :: (AliasId)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonBlankAndLengthConstraintString)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "RoutingStrategy" :: NullOrUndefined.NullOrUndefined (RoutingStrategy)
  }
derive instance newtypeUpdateAliasInput :: Newtype UpdateAliasInput _
derive instance repGenericUpdateAliasInput :: Generic UpdateAliasInput _
instance showUpdateAliasInput :: Show UpdateAliasInput where
  show = genericShow
instance decodeUpdateAliasInput :: Decode UpdateAliasInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAliasInput :: Encode UpdateAliasInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateAliasOutput = UpdateAliasOutput 
  { "Alias" :: NullOrUndefined.NullOrUndefined (Alias)
  }
derive instance newtypeUpdateAliasOutput :: Newtype UpdateAliasOutput _
derive instance repGenericUpdateAliasOutput :: Generic UpdateAliasOutput _
instance showUpdateAliasOutput :: Show UpdateAliasOutput where
  show = genericShow
instance decodeUpdateAliasOutput :: Decode UpdateAliasOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAliasOutput :: Encode UpdateAliasOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateBuildInput = UpdateBuildInput 
  { "BuildId" :: (BuildId)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Version" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeUpdateBuildInput :: Newtype UpdateBuildInput _
derive instance repGenericUpdateBuildInput :: Generic UpdateBuildInput _
instance showUpdateBuildInput :: Show UpdateBuildInput where
  show = genericShow
instance decodeUpdateBuildInput :: Decode UpdateBuildInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBuildInput :: Encode UpdateBuildInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateBuildOutput = UpdateBuildOutput 
  { "Build" :: NullOrUndefined.NullOrUndefined (Build)
  }
derive instance newtypeUpdateBuildOutput :: Newtype UpdateBuildOutput _
derive instance repGenericUpdateBuildOutput :: Generic UpdateBuildOutput _
instance showUpdateBuildOutput :: Show UpdateBuildOutput where
  show = genericShow
instance decodeUpdateBuildOutput :: Decode UpdateBuildOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBuildOutput :: Encode UpdateBuildOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateFleetAttributesInput = UpdateFleetAttributesInput 
  { "FleetId" :: (FleetId)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "NewGameSessionProtectionPolicy" :: NullOrUndefined.NullOrUndefined (ProtectionPolicy)
  , "ResourceCreationLimitPolicy" :: NullOrUndefined.NullOrUndefined (ResourceCreationLimitPolicy)
  , "MetricGroups" :: NullOrUndefined.NullOrUndefined (MetricGroupList)
  }
derive instance newtypeUpdateFleetAttributesInput :: Newtype UpdateFleetAttributesInput _
derive instance repGenericUpdateFleetAttributesInput :: Generic UpdateFleetAttributesInput _
instance showUpdateFleetAttributesInput :: Show UpdateFleetAttributesInput where
  show = genericShow
instance decodeUpdateFleetAttributesInput :: Decode UpdateFleetAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFleetAttributesInput :: Encode UpdateFleetAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateFleetAttributesOutput = UpdateFleetAttributesOutput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  }
derive instance newtypeUpdateFleetAttributesOutput :: Newtype UpdateFleetAttributesOutput _
derive instance repGenericUpdateFleetAttributesOutput :: Generic UpdateFleetAttributesOutput _
instance showUpdateFleetAttributesOutput :: Show UpdateFleetAttributesOutput where
  show = genericShow
instance decodeUpdateFleetAttributesOutput :: Decode UpdateFleetAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFleetAttributesOutput :: Encode UpdateFleetAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateFleetCapacityInput = UpdateFleetCapacityInput 
  { "FleetId" :: (FleetId)
  , "DesiredInstances" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "MinSize" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "MaxSize" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeUpdateFleetCapacityInput :: Newtype UpdateFleetCapacityInput _
derive instance repGenericUpdateFleetCapacityInput :: Generic UpdateFleetCapacityInput _
instance showUpdateFleetCapacityInput :: Show UpdateFleetCapacityInput where
  show = genericShow
instance decodeUpdateFleetCapacityInput :: Decode UpdateFleetCapacityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFleetCapacityInput :: Encode UpdateFleetCapacityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateFleetCapacityOutput = UpdateFleetCapacityOutput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  }
derive instance newtypeUpdateFleetCapacityOutput :: Newtype UpdateFleetCapacityOutput _
derive instance repGenericUpdateFleetCapacityOutput :: Generic UpdateFleetCapacityOutput _
instance showUpdateFleetCapacityOutput :: Show UpdateFleetCapacityOutput where
  show = genericShow
instance decodeUpdateFleetCapacityOutput :: Decode UpdateFleetCapacityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFleetCapacityOutput :: Encode UpdateFleetCapacityOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateFleetPortSettingsInput = UpdateFleetPortSettingsInput 
  { "FleetId" :: (FleetId)
  , "InboundPermissionAuthorizations" :: NullOrUndefined.NullOrUndefined (IpPermissionsList)
  , "InboundPermissionRevocations" :: NullOrUndefined.NullOrUndefined (IpPermissionsList)
  }
derive instance newtypeUpdateFleetPortSettingsInput :: Newtype UpdateFleetPortSettingsInput _
derive instance repGenericUpdateFleetPortSettingsInput :: Generic UpdateFleetPortSettingsInput _
instance showUpdateFleetPortSettingsInput :: Show UpdateFleetPortSettingsInput where
  show = genericShow
instance decodeUpdateFleetPortSettingsInput :: Decode UpdateFleetPortSettingsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFleetPortSettingsInput :: Encode UpdateFleetPortSettingsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateFleetPortSettingsOutput = UpdateFleetPortSettingsOutput 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  }
derive instance newtypeUpdateFleetPortSettingsOutput :: Newtype UpdateFleetPortSettingsOutput _
derive instance repGenericUpdateFleetPortSettingsOutput :: Generic UpdateFleetPortSettingsOutput _
instance showUpdateFleetPortSettingsOutput :: Show UpdateFleetPortSettingsOutput where
  show = genericShow
instance decodeUpdateFleetPortSettingsOutput :: Decode UpdateFleetPortSettingsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFleetPortSettingsOutput :: Encode UpdateFleetPortSettingsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateGameSessionInput = UpdateGameSessionInput 
  { "GameSessionId" :: (ArnStringModel)
  , "MaximumPlayerSessionCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PlayerSessionCreationPolicy" :: NullOrUndefined.NullOrUndefined (PlayerSessionCreationPolicy)
  , "ProtectionPolicy" :: NullOrUndefined.NullOrUndefined (ProtectionPolicy)
  }
derive instance newtypeUpdateGameSessionInput :: Newtype UpdateGameSessionInput _
derive instance repGenericUpdateGameSessionInput :: Generic UpdateGameSessionInput _
instance showUpdateGameSessionInput :: Show UpdateGameSessionInput where
  show = genericShow
instance decodeUpdateGameSessionInput :: Decode UpdateGameSessionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGameSessionInput :: Encode UpdateGameSessionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateGameSessionOutput = UpdateGameSessionOutput 
  { "GameSession" :: NullOrUndefined.NullOrUndefined (GameSession)
  }
derive instance newtypeUpdateGameSessionOutput :: Newtype UpdateGameSessionOutput _
derive instance repGenericUpdateGameSessionOutput :: Generic UpdateGameSessionOutput _
instance showUpdateGameSessionOutput :: Show UpdateGameSessionOutput where
  show = genericShow
instance decodeUpdateGameSessionOutput :: Decode UpdateGameSessionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGameSessionOutput :: Encode UpdateGameSessionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateGameSessionQueueInput = UpdateGameSessionQueueInput 
  { "Name" :: (GameSessionQueueName)
  , "TimeoutInSeconds" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "PlayerLatencyPolicies" :: NullOrUndefined.NullOrUndefined (PlayerLatencyPolicyList)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (GameSessionQueueDestinationList)
  }
derive instance newtypeUpdateGameSessionQueueInput :: Newtype UpdateGameSessionQueueInput _
derive instance repGenericUpdateGameSessionQueueInput :: Generic UpdateGameSessionQueueInput _
instance showUpdateGameSessionQueueInput :: Show UpdateGameSessionQueueInput where
  show = genericShow
instance decodeUpdateGameSessionQueueInput :: Decode UpdateGameSessionQueueInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGameSessionQueueInput :: Encode UpdateGameSessionQueueInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateGameSessionQueueOutput = UpdateGameSessionQueueOutput 
  { "GameSessionQueue" :: NullOrUndefined.NullOrUndefined (GameSessionQueue)
  }
derive instance newtypeUpdateGameSessionQueueOutput :: Newtype UpdateGameSessionQueueOutput _
derive instance repGenericUpdateGameSessionQueueOutput :: Generic UpdateGameSessionQueueOutput _
instance showUpdateGameSessionQueueOutput :: Show UpdateGameSessionQueueOutput where
  show = genericShow
instance decodeUpdateGameSessionQueueOutput :: Decode UpdateGameSessionQueueOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGameSessionQueueOutput :: Encode UpdateGameSessionQueueOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateMatchmakingConfigurationInput = UpdateMatchmakingConfigurationInput 
  { "Name" :: (MatchmakingIdStringModel)
  , "Description" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameSessionQueueArns" :: NullOrUndefined.NullOrUndefined (QueueArnsList)
  , "RequestTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (MatchmakingRequestTimeoutInteger)
  , "AcceptanceTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (MatchmakingAcceptanceTimeoutInteger)
  , "AcceptanceRequired" :: NullOrUndefined.NullOrUndefined (BooleanModel)
  , "RuleSetName" :: NullOrUndefined.NullOrUndefined (MatchmakingIdStringModel)
  , "NotificationTarget" :: NullOrUndefined.NullOrUndefined (SnsArnStringModel)
  , "AdditionalPlayerCount" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "CustomEventData" :: NullOrUndefined.NullOrUndefined (CustomEventData)
  , "GameProperties" :: NullOrUndefined.NullOrUndefined (GamePropertyList)
  , "GameSessionData" :: NullOrUndefined.NullOrUndefined (GameSessionData)
  }
derive instance newtypeUpdateMatchmakingConfigurationInput :: Newtype UpdateMatchmakingConfigurationInput _
derive instance repGenericUpdateMatchmakingConfigurationInput :: Generic UpdateMatchmakingConfigurationInput _
instance showUpdateMatchmakingConfigurationInput :: Show UpdateMatchmakingConfigurationInput where
  show = genericShow
instance decodeUpdateMatchmakingConfigurationInput :: Decode UpdateMatchmakingConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMatchmakingConfigurationInput :: Encode UpdateMatchmakingConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateMatchmakingConfigurationOutput = UpdateMatchmakingConfigurationOutput 
  { "Configuration" :: NullOrUndefined.NullOrUndefined (MatchmakingConfiguration)
  }
derive instance newtypeUpdateMatchmakingConfigurationOutput :: Newtype UpdateMatchmakingConfigurationOutput _
derive instance repGenericUpdateMatchmakingConfigurationOutput :: Generic UpdateMatchmakingConfigurationOutput _
instance showUpdateMatchmakingConfigurationOutput :: Show UpdateMatchmakingConfigurationOutput where
  show = genericShow
instance decodeUpdateMatchmakingConfigurationOutput :: Decode UpdateMatchmakingConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMatchmakingConfigurationOutput :: Encode UpdateMatchmakingConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype UpdateRuntimeConfigurationInput = UpdateRuntimeConfigurationInput 
  { "FleetId" :: (FleetId)
  , "RuntimeConfiguration" :: (RuntimeConfiguration)
  }
derive instance newtypeUpdateRuntimeConfigurationInput :: Newtype UpdateRuntimeConfigurationInput _
derive instance repGenericUpdateRuntimeConfigurationInput :: Generic UpdateRuntimeConfigurationInput _
instance showUpdateRuntimeConfigurationInput :: Show UpdateRuntimeConfigurationInput where
  show = genericShow
instance decodeUpdateRuntimeConfigurationInput :: Decode UpdateRuntimeConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRuntimeConfigurationInput :: Encode UpdateRuntimeConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype UpdateRuntimeConfigurationOutput = UpdateRuntimeConfigurationOutput 
  { "RuntimeConfiguration" :: NullOrUndefined.NullOrUndefined (RuntimeConfiguration)
  }
derive instance newtypeUpdateRuntimeConfigurationOutput :: Newtype UpdateRuntimeConfigurationOutput _
derive instance repGenericUpdateRuntimeConfigurationOutput :: Generic UpdateRuntimeConfigurationOutput _
instance showUpdateRuntimeConfigurationOutput :: Show UpdateRuntimeConfigurationOutput where
  show = genericShow
instance decodeUpdateRuntimeConfigurationOutput :: Decode UpdateRuntimeConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRuntimeConfigurationOutput :: Encode UpdateRuntimeConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input for a request action.</p>
newtype ValidateMatchmakingRuleSetInput = ValidateMatchmakingRuleSetInput 
  { "RuleSetBody" :: (RuleSetBody)
  }
derive instance newtypeValidateMatchmakingRuleSetInput :: Newtype ValidateMatchmakingRuleSetInput _
derive instance repGenericValidateMatchmakingRuleSetInput :: Generic ValidateMatchmakingRuleSetInput _
instance showValidateMatchmakingRuleSetInput :: Show ValidateMatchmakingRuleSetInput where
  show = genericShow
instance decodeValidateMatchmakingRuleSetInput :: Decode ValidateMatchmakingRuleSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidateMatchmakingRuleSetInput :: Encode ValidateMatchmakingRuleSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the returned data in response to a request action.</p>
newtype ValidateMatchmakingRuleSetOutput = ValidateMatchmakingRuleSetOutput 
  { "Valid" :: NullOrUndefined.NullOrUndefined (BooleanModel)
  }
derive instance newtypeValidateMatchmakingRuleSetOutput :: Newtype ValidateMatchmakingRuleSetOutput _
derive instance repGenericValidateMatchmakingRuleSetOutput :: Generic ValidateMatchmakingRuleSetOutput _
instance showValidateMatchmakingRuleSetOutput :: Show ValidateMatchmakingRuleSetOutput where
  show = genericShow
instance decodeValidateMatchmakingRuleSetOutput :: Decode ValidateMatchmakingRuleSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidateMatchmakingRuleSetOutput :: Encode ValidateMatchmakingRuleSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an authorization for a VPC peering connection between the VPC for an Amazon GameLift fleet and another VPC on an account you have access to. This authorization must exist and be valid for the peering connection to be established. Authorizations are valid for 24 hours after they are issued.</p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
newtype VpcPeeringAuthorization = VpcPeeringAuthorization 
  { "GameLiftAwsAccountId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PeerVpcAwsAccountId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "PeerVpcId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "ExpirationTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeVpcPeeringAuthorization :: Newtype VpcPeeringAuthorization _
derive instance repGenericVpcPeeringAuthorization :: Generic VpcPeeringAuthorization _
instance showVpcPeeringAuthorization :: Show VpcPeeringAuthorization where
  show = genericShow
instance decodeVpcPeeringAuthorization :: Decode VpcPeeringAuthorization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcPeeringAuthorization :: Encode VpcPeeringAuthorization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcPeeringAuthorizationList = VpcPeeringAuthorizationList (Array VpcPeeringAuthorization)
derive instance newtypeVpcPeeringAuthorizationList :: Newtype VpcPeeringAuthorizationList _
derive instance repGenericVpcPeeringAuthorizationList :: Generic VpcPeeringAuthorizationList _
instance showVpcPeeringAuthorizationList :: Show VpcPeeringAuthorizationList where
  show = genericShow
instance decodeVpcPeeringAuthorizationList :: Decode VpcPeeringAuthorizationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcPeeringAuthorizationList :: Encode VpcPeeringAuthorizationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a peering connection between a VPC on one of your AWS accounts and the VPC for your Amazon GameLift fleets. This record may be for an active peering connection or a pending connection that has not yet been established.</p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>
newtype VpcPeeringConnection = VpcPeeringConnection 
  { "FleetId" :: NullOrUndefined.NullOrUndefined (FleetId)
  , "IpV4CidrBlock" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "VpcPeeringConnectionId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Status" :: NullOrUndefined.NullOrUndefined (VpcPeeringConnectionStatus)
  , "PeerVpcId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "GameLiftVpcId" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeVpcPeeringConnection :: Newtype VpcPeeringConnection _
derive instance repGenericVpcPeeringConnection :: Generic VpcPeeringConnection _
instance showVpcPeeringConnection :: Show VpcPeeringConnection where
  show = genericShow
instance decodeVpcPeeringConnection :: Decode VpcPeeringConnection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcPeeringConnection :: Encode VpcPeeringConnection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcPeeringConnectionList = VpcPeeringConnectionList (Array VpcPeeringConnection)
derive instance newtypeVpcPeeringConnectionList :: Newtype VpcPeeringConnectionList _
derive instance repGenericVpcPeeringConnectionList :: Generic VpcPeeringConnectionList _
instance showVpcPeeringConnectionList :: Show VpcPeeringConnectionList where
  show = genericShow
instance decodeVpcPeeringConnectionList :: Decode VpcPeeringConnectionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcPeeringConnectionList :: Encode VpcPeeringConnectionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents status information for a VPC peering connection. Status is associated with a <a>VpcPeeringConnection</a> object. Status codes and messages are provided from EC2 (see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html">VpcPeeringConnectionStateReason</a>). Connection status information is also communicated as a fleet <a>Event</a>.</p>
newtype VpcPeeringConnectionStatus = VpcPeeringConnectionStatus 
  { "Code" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  , "Message" :: NullOrUndefined.NullOrUndefined (NonZeroAndMaxString)
  }
derive instance newtypeVpcPeeringConnectionStatus :: Newtype VpcPeeringConnectionStatus _
derive instance repGenericVpcPeeringConnectionStatus :: Generic VpcPeeringConnectionStatus _
instance showVpcPeeringConnectionStatus :: Show VpcPeeringConnectionStatus where
  show = genericShow
instance decodeVpcPeeringConnectionStatus :: Decode VpcPeeringConnectionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcPeeringConnectionStatus :: Encode VpcPeeringConnectionStatus where
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
