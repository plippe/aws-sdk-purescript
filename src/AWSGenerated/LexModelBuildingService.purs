

-- | <fullname>Amazon Lex Build-Time Actions</fullname> <p> Amazon Lex is an AWS service for building conversational voice and text interfaces. Use these actions to create, update, and delete conversational bots for new and existing client applications. </p>
module AWS.LexModelBuildingService where

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

serviceName = "LexModelBuildingService" :: String


-- | <p>Creates a new version of the bot based on the <code>$LATEST</code> version. If the <code>$LATEST</code> version of this resource hasn't changed since you created the last version, Amazon Lex doesn't create a new version. It returns the last created version.</p> <note> <p>You can update only the <code>$LATEST</code> version of the bot. You can't update the numbered versions that you create with the <code>CreateBotVersion</code> operation.</p> </note> <p> When you create the first version of a bot, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see <a>versioning-intro</a>. </p> <p> This operation requires permission for the <code>lex:CreateBotVersion</code> action. </p>
createBotVersion :: forall eff. CreateBotVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateBotVersionResponse
createBotVersion = Request.request serviceName "createBotVersion" 


-- | <p>Creates a new version of an intent based on the <code>$LATEST</code> version of the intent. If the <code>$LATEST</code> version of this intent hasn't changed since you last updated it, Amazon Lex doesn't create a new version. It returns the last version you created.</p> <note> <p>You can update only the <code>$LATEST</code> version of the intent. You can't update the numbered versions that you create with the <code>CreateIntentVersion</code> operation.</p> </note> <p> When you create a version of an intent, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see <a>versioning-intro</a>. </p> <p>This operation requires permissions to perform the <code>lex:CreateIntentVersion</code> action. </p>
createIntentVersion :: forall eff. CreateIntentVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateIntentVersionResponse
createIntentVersion = Request.request serviceName "createIntentVersion" 


-- | <p>Creates a new version of a slot type based on the <code>$LATEST</code> version of the specified slot type. If the <code>$LATEST</code> version of this resource has not changed since the last version that you created, Amazon Lex doesn't create a new version. It returns the last version that you created. </p> <note> <p>You can update only the <code>$LATEST</code> version of a slot type. You can't update the numbered versions that you create with the <code>CreateSlotTypeVersion</code> operation.</p> </note> <p>When you create a version of a slot type, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see <a>versioning-intro</a>. </p> <p>This operation requires permissions for the <code>lex:CreateSlotTypeVersion</code> action.</p>
createSlotTypeVersion :: forall eff. CreateSlotTypeVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateSlotTypeVersionResponse
createSlotTypeVersion = Request.request serviceName "createSlotTypeVersion" 


-- | <p>Deletes all versions of the bot, including the <code>$LATEST</code> version. To delete a specific version of the bot, use the <a>DeleteBotVersion</a> operation.</p> <p>If a bot has an alias, you can't delete it. Instead, the <code>DeleteBot</code> operation returns a <code>ResourceInUseException</code> exception that includes a reference to the alias that refers to the bot. To remove the reference to the bot, delete the alias. If you get the same exception again, delete the referring alias until the <code>DeleteBot</code> operation is successful.</p> <p>This operation requires permissions for the <code>lex:DeleteBot</code> action.</p>
deleteBot :: forall eff. DeleteBotRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBot = Request.request serviceName "deleteBot" 


-- | <p>Deletes an alias for the specified bot. </p> <p>You can't delete an alias that is used in the association between a bot and a messaging channel. If an alias is used in a channel association, the <code>DeleteBot</code> operation returns a <code>ResourceInUseException</code> exception that includes a reference to the channel association that refers to the bot. You can remove the reference to the alias by deleting the channel association. If you get the same exception again, delete the referring association until the <code>DeleteBotAlias</code> operation is successful.</p>
deleteBotAlias :: forall eff. DeleteBotAliasRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBotAlias = Request.request serviceName "deleteBotAlias" 


-- | <p>Deletes the association between an Amazon Lex bot and a messaging platform.</p> <p>This operation requires permission for the <code>lex:DeleteBotChannelAssociation</code> action.</p>
deleteBotChannelAssociation :: forall eff. DeleteBotChannelAssociationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBotChannelAssociation = Request.request serviceName "deleteBotChannelAssociation" 


-- | <p>Deletes a specific version of a bot. To delete all versions of a bot, use the <a>DeleteBot</a> operation. </p> <p>This operation requires permissions for the <code>lex:DeleteBotVersion</code> action.</p>
deleteBotVersion :: forall eff. DeleteBotVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteBotVersion = Request.request serviceName "deleteBotVersion" 


-- | <p>Deletes all versions of the intent, including the <code>$LATEST</code> version. To delete a specific version of the intent, use the <a>DeleteIntentVersion</a> operation.</p> <p> You can delete a version of an intent only if it is not referenced. To delete an intent that is referred to in one or more bots (see <a>how-it-works</a>), you must remove those references first. </p> <note> <p> If you get the <code>ResourceInUseException</code> exception, it provides an example reference that shows where the intent is referenced. To remove the reference to the intent, either update the bot or delete it. If you get the same exception when you attempt to delete the intent again, repeat until the intent has no references and the call to <code>DeleteIntent</code> is successful. </p> </note> <p> This operation requires permission for the <code>lex:DeleteIntent</code> action. </p>
deleteIntent :: forall eff. DeleteIntentRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteIntent = Request.request serviceName "deleteIntent" 


-- | <p>Deletes a specific version of an intent. To delete all versions of a intent, use the <a>DeleteIntent</a> operation. </p> <p>This operation requires permissions for the <code>lex:DeleteIntentVersion</code> action.</p>
deleteIntentVersion :: forall eff. DeleteIntentVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteIntentVersion = Request.request serviceName "deleteIntentVersion" 


-- | <p>Deletes all versions of the slot type, including the <code>$LATEST</code> version. To delete a specific version of the slot type, use the <a>DeleteSlotTypeVersion</a> operation.</p> <p> You can delete a version of a slot type only if it is not referenced. To delete a slot type that is referred to in one or more intents, you must remove those references first. </p> <note> <p> If you get the <code>ResourceInUseException</code> exception, the exception provides an example reference that shows the intent where the slot type is referenced. To remove the reference to the slot type, either update the intent or delete it. If you get the same exception when you attempt to delete the slot type again, repeat until the slot type has no references and the <code>DeleteSlotType</code> call is successful. </p> </note> <p>This operation requires permission for the <code>lex:DeleteSlotType</code> action.</p>
deleteSlotType :: forall eff. DeleteSlotTypeRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteSlotType = Request.request serviceName "deleteSlotType" 


-- | <p>Deletes a specific version of a slot type. To delete all versions of a slot type, use the <a>DeleteSlotType</a> operation. </p> <p>This operation requires permissions for the <code>lex:DeleteSlotTypeVersion</code> action.</p>
deleteSlotTypeVersion :: forall eff. DeleteSlotTypeVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteSlotTypeVersion = Request.request serviceName "deleteSlotTypeVersion" 


-- | <p>Deletes stored utterances.</p> <p>Amazon Lex stores the utterances that users send to your bot. Utterances are stored for 15 days for use with the <a>GetUtterancesView</a> operation, and then stored indefinitely for use in improving the ability of your bot to respond to user input.</p> <p>Use the <code>DeleteStoredUtterances</code> operation to manually delete stored utterances for a specific user.</p> <p>This operation requires permissions for the <code>lex:DeleteUtterances</code> action.</p>
deleteUtterances :: forall eff. DeleteUtterancesRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUtterances = Request.request serviceName "deleteUtterances" 


-- | <p>Returns metadata information for a specific bot. You must provide the bot name and the bot version or alias. </p> <p> This operation requires permissions for the <code>lex:GetBot</code> action. </p>
getBot :: forall eff. GetBotRequest -> Aff (exception :: EXCEPTION | eff) GetBotResponse
getBot = Request.request serviceName "getBot" 


-- | <p>Returns information about an Amazon Lex bot alias. For more information about aliases, see <a>versioning-aliases</a>.</p> <p>This operation requires permissions for the <code>lex:GetBotAlias</code> action.</p>
getBotAlias :: forall eff. GetBotAliasRequest -> Aff (exception :: EXCEPTION | eff) GetBotAliasResponse
getBotAlias = Request.request serviceName "getBotAlias" 


-- | <p>Returns a list of aliases for a specified Amazon Lex bot.</p> <p>This operation requires permissions for the <code>lex:GetBotAliases</code> action.</p>
getBotAliases :: forall eff. GetBotAliasesRequest -> Aff (exception :: EXCEPTION | eff) GetBotAliasesResponse
getBotAliases = Request.request serviceName "getBotAliases" 


-- | <p>Returns information about the association between an Amazon Lex bot and a messaging platform.</p> <p>This operation requires permissions for the <code>lex:GetBotChannelAssociation</code> action.</p>
getBotChannelAssociation :: forall eff. GetBotChannelAssociationRequest -> Aff (exception :: EXCEPTION | eff) GetBotChannelAssociationResponse
getBotChannelAssociation = Request.request serviceName "getBotChannelAssociation" 


-- | <p> Returns a list of all of the channels associated with the specified bot. </p> <p>The <code>GetBotChannelAssociations</code> operation requires permissions for the <code>lex:GetBotChannelAssociations</code> action.</p>
getBotChannelAssociations :: forall eff. GetBotChannelAssociationsRequest -> Aff (exception :: EXCEPTION | eff) GetBotChannelAssociationsResponse
getBotChannelAssociations = Request.request serviceName "getBotChannelAssociations" 


-- | <p>Gets information about all of the versions of a bot.</p> <p>The <code>GetBotVersions</code> operation returns a <code>BotMetadata</code> object for each version of a bot. For example, if a bot has three numbered versions, the <code>GetBotVersions</code> operation returns four <code>BotMetadata</code> objects in the response, one for each numbered version and one for the <code>$LATEST</code> version. </p> <p>The <code>GetBotVersions</code> operation always returns at least one version, the <code>$LATEST</code> version.</p> <p>This operation requires permissions for the <code>lex:GetBotVersions</code> action.</p>
getBotVersions :: forall eff. GetBotVersionsRequest -> Aff (exception :: EXCEPTION | eff) GetBotVersionsResponse
getBotVersions = Request.request serviceName "getBotVersions" 


-- | <p>Returns bot information as follows: </p> <ul> <li> <p>If you provide the <code>nameContains</code> field, the response includes information for the <code>$LATEST</code> version of all bots whose name contains the specified string.</p> </li> <li> <p>If you don't specify the <code>nameContains</code> field, the operation returns information about the <code>$LATEST</code> version of all of your bots.</p> </li> </ul> <p>This operation requires permission for the <code>lex:GetBots</code> action.</p>
getBots :: forall eff. GetBotsRequest -> Aff (exception :: EXCEPTION | eff) GetBotsResponse
getBots = Request.request serviceName "getBots" 


-- | <p>Returns information about a built-in intent.</p> <p>This operation requires permission for the <code>lex:GetBuiltinIntent</code> action.</p>
getBuiltinIntent :: forall eff. GetBuiltinIntentRequest -> Aff (exception :: EXCEPTION | eff) GetBuiltinIntentResponse
getBuiltinIntent = Request.request serviceName "getBuiltinIntent" 


-- | <p>Gets a list of built-in intents that meet the specified criteria.</p> <p>This operation requires permission for the <code>lex:GetBuiltinIntents</code> action.</p>
getBuiltinIntents :: forall eff. GetBuiltinIntentsRequest -> Aff (exception :: EXCEPTION | eff) GetBuiltinIntentsResponse
getBuiltinIntents = Request.request serviceName "getBuiltinIntents" 


-- | <p>Gets a list of built-in slot types that meet the specified criteria.</p> <p>For a list of built-in slot types, see <a href="https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference">Slot Type Reference</a> in the <i>Alexa Skills Kit</i>.</p> <p>This operation requires permission for the <code>lex:GetBuiltInSlotTypes</code> action.</p>
getBuiltinSlotTypes :: forall eff. GetBuiltinSlotTypesRequest -> Aff (exception :: EXCEPTION | eff) GetBuiltinSlotTypesResponse
getBuiltinSlotTypes = Request.request serviceName "getBuiltinSlotTypes" 


-- | <p>Exports the contents of a Amazon Lex resource in a specified format. </p>
getExport :: forall eff. GetExportRequest -> Aff (exception :: EXCEPTION | eff) GetExportResponse
getExport = Request.request serviceName "getExport" 


-- | <p>Gets information about an import job started with the <code>StartImport</code> operation.</p>
getImport :: forall eff. GetImportRequest -> Aff (exception :: EXCEPTION | eff) GetImportResponse
getImport = Request.request serviceName "getImport" 


-- | <p> Returns information about an intent. In addition to the intent name, you must specify the intent version. </p> <p> This operation requires permissions to perform the <code>lex:GetIntent</code> action. </p>
getIntent :: forall eff. GetIntentRequest -> Aff (exception :: EXCEPTION | eff) GetIntentResponse
getIntent = Request.request serviceName "getIntent" 


-- | <p>Gets information about all of the versions of an intent.</p> <p>The <code>GetIntentVersions</code> operation returns an <code>IntentMetadata</code> object for each version of an intent. For example, if an intent has three numbered versions, the <code>GetIntentVersions</code> operation returns four <code>IntentMetadata</code> objects in the response, one for each numbered version and one for the <code>$LATEST</code> version. </p> <p>The <code>GetIntentVersions</code> operation always returns at least one version, the <code>$LATEST</code> version.</p> <p>This operation requires permissions for the <code>lex:GetIntentVersions</code> action.</p>
getIntentVersions :: forall eff. GetIntentVersionsRequest -> Aff (exception :: EXCEPTION | eff) GetIntentVersionsResponse
getIntentVersions = Request.request serviceName "getIntentVersions" 


-- | <p>Returns intent information as follows: </p> <ul> <li> <p>If you specify the <code>nameContains</code> field, returns the <code>$LATEST</code> version of all intents that contain the specified string.</p> </li> <li> <p> If you don't specify the <code>nameContains</code> field, returns information about the <code>$LATEST</code> version of all intents. </p> </li> </ul> <p> The operation requires permission for the <code>lex:GetIntents</code> action. </p>
getIntents :: forall eff. GetIntentsRequest -> Aff (exception :: EXCEPTION | eff) GetIntentsResponse
getIntents = Request.request serviceName "getIntents" 


-- | <p>Returns information about a specific version of a slot type. In addition to specifying the slot type name, you must specify the slot type version.</p> <p>This operation requires permissions for the <code>lex:GetSlotType</code> action.</p>
getSlotType :: forall eff. GetSlotTypeRequest -> Aff (exception :: EXCEPTION | eff) GetSlotTypeResponse
getSlotType = Request.request serviceName "getSlotType" 


-- | <p>Gets information about all versions of a slot type.</p> <p>The <code>GetSlotTypeVersions</code> operation returns a <code>SlotTypeMetadata</code> object for each version of a slot type. For example, if a slot type has three numbered versions, the <code>GetSlotTypeVersions</code> operation returns four <code>SlotTypeMetadata</code> objects in the response, one for each numbered version and one for the <code>$LATEST</code> version. </p> <p>The <code>GetSlotTypeVersions</code> operation always returns at least one version, the <code>$LATEST</code> version.</p> <p>This operation requires permissions for the <code>lex:GetSlotTypeVersions</code> action.</p>
getSlotTypeVersions :: forall eff. GetSlotTypeVersionsRequest -> Aff (exception :: EXCEPTION | eff) GetSlotTypeVersionsResponse
getSlotTypeVersions = Request.request serviceName "getSlotTypeVersions" 


-- | <p>Returns slot type information as follows: </p> <ul> <li> <p>If you specify the <code>nameContains</code> field, returns the <code>$LATEST</code> version of all slot types that contain the specified string.</p> </li> <li> <p> If you don't specify the <code>nameContains</code> field, returns information about the <code>$LATEST</code> version of all slot types. </p> </li> </ul> <p> The operation requires permission for the <code>lex:GetSlotTypes</code> action. </p>
getSlotTypes :: forall eff. GetSlotTypesRequest -> Aff (exception :: EXCEPTION | eff) GetSlotTypesResponse
getSlotTypes = Request.request serviceName "getSlotTypes" 


-- | <p>Use the <code>GetUtterancesView</code> operation to get information about the utterances that your users have made to your bot. You can use this list to tune the utterances that your bot responds to.</p> <p>For example, say that you have created a bot to order flowers. After your users have used your bot for a while, use the <code>GetUtterancesView</code> operation to see the requests that they have made and whether they have been successful. You might find that the utterance "I want flowers" is not being recognized. You could add this utterance to the <code>OrderFlowers</code> intent so that your bot recognizes that utterance.</p> <p>After you publish a new version of a bot, you can get information about the old version and the new so that you can compare the performance across the two versions. </p> <note> <p>Utterance statistics are generated once a day. Data is available for the last 15 days. You can request information for up to 5 versions in each request. The response contains information about a maximum of 100 utterances for each version.</p> </note> <p>This operation requires permissions for the <code>lex:GetUtterancesView</code> action.</p>
getUtterancesView :: forall eff. GetUtterancesViewRequest -> Aff (exception :: EXCEPTION | eff) GetUtterancesViewResponse
getUtterancesView = Request.request serviceName "getUtterancesView" 


-- | <p>Creates an Amazon Lex conversational bot or replaces an existing bot. When you create or update a bot you are only required to specify a name, a locale, and whether the bot is directed toward children under age 13. You can use this to add intents later, or to remove intents from an existing bot. When you create a bot with the minimum information, the bot is created or updated but Amazon Lex returns the <code/> response <code>FAILED</code>. You can build the bot after you add one or more intents. For more information about Amazon Lex bots, see <a>how-it-works</a>. </p> <p>If you specify the name of an existing bot, the fields in the request replace the existing values in the <code>$LATEST</code> version of the bot. Amazon Lex removes any fields that you don't provide values for in the request, except for the <code>idleTTLInSeconds</code> and <code>privacySettings</code> fields, which are set to their default values. If you don't specify values for required fields, Amazon Lex throws an exception.</p> <p>This operation requires permissions for the <code>lex:PutBot</code> action. For more information, see <a>auth-and-access-control</a>.</p>
putBot :: forall eff. PutBotRequest -> Aff (exception :: EXCEPTION | eff) PutBotResponse
putBot = Request.request serviceName "putBot" 


-- | <p>Creates an alias for the specified version of the bot or replaces an alias for the specified bot. To change the version of the bot that the alias points to, replace the alias. For more information about aliases, see <a>versioning-aliases</a>.</p> <p>This operation requires permissions for the <code>lex:PutBotAlias</code> action. </p>
putBotAlias :: forall eff. PutBotAliasRequest -> Aff (exception :: EXCEPTION | eff) PutBotAliasResponse
putBotAlias = Request.request serviceName "putBotAlias" 


-- | <p>Creates an intent or replaces an existing intent.</p> <p>To define the interaction between the user and your bot, you use one or more intents. For a pizza ordering bot, for example, you would create an <code>OrderPizza</code> intent. </p> <p>To create an intent or replace an existing intent, you must provide the following:</p> <ul> <li> <p>Intent name. For example, <code>OrderPizza</code>.</p> </li> <li> <p>Sample utterances. For example, "Can I order a pizza, please." and "I want to order a pizza."</p> </li> <li> <p>Information to be gathered. You specify slot types for the information that your bot will request from the user. You can specify standard slot types, such as a date or a time, or custom slot types such as the size and crust of a pizza.</p> </li> <li> <p>How the intent will be fulfilled. You can provide a Lambda function or configure the intent to return the intent information to the client application. If you use a Lambda function, when all of the intent information is available, Amazon Lex invokes your Lambda function. If you configure your intent to return the intent information to the client application. </p> </li> </ul> <p>You can specify other optional information in the request, such as:</p> <ul> <li> <p>A confirmation prompt to ask the user to confirm an intent. For example, "Shall I order your pizza?"</p> </li> <li> <p>A conclusion statement to send to the user after the intent has been fulfilled. For example, "I placed your pizza order."</p> </li> <li> <p>A follow-up prompt that asks the user for additional activity. For example, asking "Do you want to order a drink with your pizza?"</p> </li> </ul> <p>If you specify an existing intent name to update the intent, Amazon Lex replaces the values in the <code>$LATEST</code> version of the intent with the values in the request. Amazon Lex removes fields that you don't provide in the request. If you don't specify the required fields, Amazon Lex throws an exception. When you update the <code>$LATEST</code> version of an intent, the <code>status</code> field of any bot that uses the <code>$LATEST</code> version of the intent is set to <code>NOT_BUILT</code>.</p> <p>For more information, see <a>how-it-works</a>.</p> <p>This operation requires permissions for the <code>lex:PutIntent</code> action.</p>
putIntent :: forall eff. PutIntentRequest -> Aff (exception :: EXCEPTION | eff) PutIntentResponse
putIntent = Request.request serviceName "putIntent" 


-- | <p>Creates a custom slot type or replaces an existing custom slot type.</p> <p>To create a custom slot type, specify a name for the slot type and a set of enumeration values, which are the values that a slot of this type can assume. For more information, see <a>how-it-works</a>.</p> <p>If you specify the name of an existing slot type, the fields in the request replace the existing values in the <code>$LATEST</code> version of the slot type. Amazon Lex removes the fields that you don't provide in the request. If you don't specify required fields, Amazon Lex throws an exception. When you update the <code>$LATEST</code> version of a slot type, if a bot uses the <code>$LATEST</code> version of an intent that contains the slot type, the bot's <code>status</code> field is set to <code>NOT_BUILT</code>.</p> <p>This operation requires permissions for the <code>lex:PutSlotType</code> action.</p>
putSlotType :: forall eff. PutSlotTypeRequest -> Aff (exception :: EXCEPTION | eff) PutSlotTypeResponse
putSlotType = Request.request serviceName "putSlotType" 


-- | <p>Starts a job to import a resource to Amazon Lex.</p>
startImport :: forall eff. StartImportRequest -> Aff (exception :: EXCEPTION | eff) StartImportResponse
startImport = Request.request serviceName "startImport" 


newtype AliasName = AliasName String
derive instance newtypeAliasName :: Newtype AliasName _
derive instance repGenericAliasName :: Generic AliasName _
instance showAliasName :: Show AliasName where
  show = genericShow
instance decodeAliasName :: Decode AliasName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasName :: Encode AliasName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasNameOrListAll = AliasNameOrListAll String
derive instance newtypeAliasNameOrListAll :: Newtype AliasNameOrListAll _
derive instance repGenericAliasNameOrListAll :: Generic AliasNameOrListAll _
instance showAliasNameOrListAll :: Show AliasNameOrListAll where
  show = genericShow
instance decodeAliasNameOrListAll :: Decode AliasNameOrListAll where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasNameOrListAll :: Encode AliasNameOrListAll where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again.</p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a bot alias.</p>
newtype BotAliasMetadata = BotAliasMetadata 
  { "Name'" :: NullOrUndefined.NullOrUndefined (AliasName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "BotVersion'" :: NullOrUndefined.NullOrUndefined (Version)
  , "BotName'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBotAliasMetadata :: Newtype BotAliasMetadata _
derive instance repGenericBotAliasMetadata :: Generic BotAliasMetadata _
instance showBotAliasMetadata :: Show BotAliasMetadata where
  show = genericShow
instance decodeBotAliasMetadata :: Decode BotAliasMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotAliasMetadata :: Encode BotAliasMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotAliasMetadataList = BotAliasMetadataList (Array BotAliasMetadata)
derive instance newtypeBotAliasMetadataList :: Newtype BotAliasMetadataList _
derive instance repGenericBotAliasMetadataList :: Generic BotAliasMetadataList _
instance showBotAliasMetadataList :: Show BotAliasMetadataList where
  show = genericShow
instance decodeBotAliasMetadataList :: Decode BotAliasMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotAliasMetadataList :: Encode BotAliasMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an association between an Amazon Lex bot and an external messaging platform.</p>
newtype BotChannelAssociation = BotChannelAssociation 
  { "Name'" :: NullOrUndefined.NullOrUndefined (BotChannelName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "BotAlias'" :: NullOrUndefined.NullOrUndefined (AliasName)
  , "BotName'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Type'" :: NullOrUndefined.NullOrUndefined (ChannelType)
  , "BotConfiguration'" :: NullOrUndefined.NullOrUndefined (ChannelConfigurationMap)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ChannelStatus)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBotChannelAssociation :: Newtype BotChannelAssociation _
derive instance repGenericBotChannelAssociation :: Generic BotChannelAssociation _
instance showBotChannelAssociation :: Show BotChannelAssociation where
  show = genericShow
instance decodeBotChannelAssociation :: Decode BotChannelAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotChannelAssociation :: Encode BotChannelAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotChannelAssociationList = BotChannelAssociationList (Array BotChannelAssociation)
derive instance newtypeBotChannelAssociationList :: Newtype BotChannelAssociationList _
derive instance repGenericBotChannelAssociationList :: Generic BotChannelAssociationList _
instance showBotChannelAssociationList :: Show BotChannelAssociationList where
  show = genericShow
instance decodeBotChannelAssociationList :: Decode BotChannelAssociationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotChannelAssociationList :: Encode BotChannelAssociationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotChannelName = BotChannelName String
derive instance newtypeBotChannelName :: Newtype BotChannelName _
derive instance repGenericBotChannelName :: Generic BotChannelName _
instance showBotChannelName :: Show BotChannelName where
  show = genericShow
instance decodeBotChannelName :: Decode BotChannelName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotChannelName :: Encode BotChannelName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a bot. .</p>
newtype BotMetadata = BotMetadata 
  { "Name'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Status)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeBotMetadata :: Newtype BotMetadata _
derive instance repGenericBotMetadata :: Generic BotMetadata _
instance showBotMetadata :: Show BotMetadata where
  show = genericShow
instance decodeBotMetadata :: Decode BotMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotMetadata :: Encode BotMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotMetadataList = BotMetadataList (Array BotMetadata)
derive instance newtypeBotMetadataList :: Newtype BotMetadataList _
derive instance repGenericBotMetadataList :: Generic BotMetadataList _
instance showBotMetadataList :: Show BotMetadataList where
  show = genericShow
instance decodeBotMetadataList :: Decode BotMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotMetadataList :: Encode BotMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotName = BotName String
derive instance newtypeBotName :: Newtype BotName _
derive instance repGenericBotName :: Generic BotName _
instance showBotName :: Show BotName where
  show = genericShow
instance decodeBotName :: Decode BotName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotName :: Encode BotName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotVersions = BotVersions (Array Version)
derive instance newtypeBotVersions :: Newtype BotVersions _
derive instance repGenericBotVersions :: Generic BotVersions _
instance showBotVersions :: Show BotVersions where
  show = genericShow
instance decodeBotVersions :: Decode BotVersions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotVersions :: Encode BotVersions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides metadata for a built-in intent.</p>
newtype BuiltinIntentMetadata = BuiltinIntentMetadata 
  { "Signature'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSignature)
  , "SupportedLocales'" :: NullOrUndefined.NullOrUndefined (LocaleList)
  }
derive instance newtypeBuiltinIntentMetadata :: Newtype BuiltinIntentMetadata _
derive instance repGenericBuiltinIntentMetadata :: Generic BuiltinIntentMetadata _
instance showBuiltinIntentMetadata :: Show BuiltinIntentMetadata where
  show = genericShow
instance decodeBuiltinIntentMetadata :: Decode BuiltinIntentMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinIntentMetadata :: Encode BuiltinIntentMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuiltinIntentMetadataList = BuiltinIntentMetadataList (Array BuiltinIntentMetadata)
derive instance newtypeBuiltinIntentMetadataList :: Newtype BuiltinIntentMetadataList _
derive instance repGenericBuiltinIntentMetadataList :: Generic BuiltinIntentMetadataList _
instance showBuiltinIntentMetadataList :: Show BuiltinIntentMetadataList where
  show = genericShow
instance decodeBuiltinIntentMetadataList :: Decode BuiltinIntentMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinIntentMetadataList :: Encode BuiltinIntentMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuiltinIntentSignature = BuiltinIntentSignature String
derive instance newtypeBuiltinIntentSignature :: Newtype BuiltinIntentSignature _
derive instance repGenericBuiltinIntentSignature :: Generic BuiltinIntentSignature _
instance showBuiltinIntentSignature :: Show BuiltinIntentSignature where
  show = genericShow
instance decodeBuiltinIntentSignature :: Decode BuiltinIntentSignature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinIntentSignature :: Encode BuiltinIntentSignature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a slot used in a built-in intent.</p>
newtype BuiltinIntentSlot = BuiltinIntentSlot 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBuiltinIntentSlot :: Newtype BuiltinIntentSlot _
derive instance repGenericBuiltinIntentSlot :: Generic BuiltinIntentSlot _
instance showBuiltinIntentSlot :: Show BuiltinIntentSlot where
  show = genericShow
instance decodeBuiltinIntentSlot :: Decode BuiltinIntentSlot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinIntentSlot :: Encode BuiltinIntentSlot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuiltinIntentSlotList = BuiltinIntentSlotList (Array BuiltinIntentSlot)
derive instance newtypeBuiltinIntentSlotList :: Newtype BuiltinIntentSlotList _
derive instance repGenericBuiltinIntentSlotList :: Generic BuiltinIntentSlotList _
instance showBuiltinIntentSlotList :: Show BuiltinIntentSlotList where
  show = genericShow
instance decodeBuiltinIntentSlotList :: Decode BuiltinIntentSlotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinIntentSlotList :: Encode BuiltinIntentSlotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a built in slot type.</p>
newtype BuiltinSlotTypeMetadata = BuiltinSlotTypeMetadata 
  { "Signature'" :: NullOrUndefined.NullOrUndefined (BuiltinSlotTypeSignature)
  , "SupportedLocales'" :: NullOrUndefined.NullOrUndefined (LocaleList)
  }
derive instance newtypeBuiltinSlotTypeMetadata :: Newtype BuiltinSlotTypeMetadata _
derive instance repGenericBuiltinSlotTypeMetadata :: Generic BuiltinSlotTypeMetadata _
instance showBuiltinSlotTypeMetadata :: Show BuiltinSlotTypeMetadata where
  show = genericShow
instance decodeBuiltinSlotTypeMetadata :: Decode BuiltinSlotTypeMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinSlotTypeMetadata :: Encode BuiltinSlotTypeMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuiltinSlotTypeMetadataList = BuiltinSlotTypeMetadataList (Array BuiltinSlotTypeMetadata)
derive instance newtypeBuiltinSlotTypeMetadataList :: Newtype BuiltinSlotTypeMetadataList _
derive instance repGenericBuiltinSlotTypeMetadataList :: Generic BuiltinSlotTypeMetadataList _
instance showBuiltinSlotTypeMetadataList :: Show BuiltinSlotTypeMetadataList where
  show = genericShow
instance decodeBuiltinSlotTypeMetadataList :: Decode BuiltinSlotTypeMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinSlotTypeMetadataList :: Encode BuiltinSlotTypeMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuiltinSlotTypeSignature = BuiltinSlotTypeSignature String
derive instance newtypeBuiltinSlotTypeSignature :: Newtype BuiltinSlotTypeSignature _
derive instance repGenericBuiltinSlotTypeSignature :: Generic BuiltinSlotTypeSignature _
instance showBuiltinSlotTypeSignature :: Show BuiltinSlotTypeSignature where
  show = genericShow
instance decodeBuiltinSlotTypeSignature :: Decode BuiltinSlotTypeSignature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuiltinSlotTypeSignature :: Encode BuiltinSlotTypeSignature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChannelConfigurationMap = ChannelConfigurationMap (StrMap.StrMap String)
derive instance newtypeChannelConfigurationMap :: Newtype ChannelConfigurationMap _
derive instance repGenericChannelConfigurationMap :: Generic ChannelConfigurationMap _
instance showChannelConfigurationMap :: Show ChannelConfigurationMap where
  show = genericShow
instance decodeChannelConfigurationMap :: Decode ChannelConfigurationMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelConfigurationMap :: Encode ChannelConfigurationMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChannelStatus = ChannelStatus String
derive instance newtypeChannelStatus :: Newtype ChannelStatus _
derive instance repGenericChannelStatus :: Generic ChannelStatus _
instance showChannelStatus :: Show ChannelStatus where
  show = genericShow
instance decodeChannelStatus :: Decode ChannelStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelStatus :: Encode ChannelStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChannelType = ChannelType String
derive instance newtypeChannelType :: Newtype ChannelType _
derive instance repGenericChannelType :: Generic ChannelType _
instance showChannelType :: Show ChannelType where
  show = genericShow
instance decodeChannelType :: Decode ChannelType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelType :: Encode ChannelType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a Lambda function that verifies requests to a bot or fulfills the user's request to a bot..</p>
newtype CodeHook = CodeHook 
  { "Uri'" :: (LambdaARN)
  , "MessageVersion'" :: (MessageVersion)
  }
derive instance newtypeCodeHook :: Newtype CodeHook _
derive instance repGenericCodeHook :: Generic CodeHook _
instance showCodeHook :: Show CodeHook where
  show = genericShow
instance decodeCodeHook :: Decode CodeHook where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeHook :: Encode CodeHook where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> There was a conflict processing the request. Try your request again. </p>
newtype ConflictException = ConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _
derive instance repGenericConflictException :: Generic ConflictException _
instance showConflictException :: Show ConflictException where
  show = genericShow
instance decodeConflictException :: Decode ConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictException :: Encode ConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentString = ContentString String
derive instance newtypeContentString :: Newtype ContentString _
derive instance repGenericContentString :: Generic ContentString _
instance showContentString :: Show ContentString where
  show = genericShow
instance decodeContentString :: Decode ContentString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentString :: Encode ContentString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _
derive instance repGenericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where
  show = genericShow
instance decodeContentType :: Decode ContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentType :: Encode ContentType where
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


newtype CreateBotVersionRequest = CreateBotVersionRequest 
  { "Name'" :: (BotName)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateBotVersionRequest :: Newtype CreateBotVersionRequest _
derive instance repGenericCreateBotVersionRequest :: Generic CreateBotVersionRequest _
instance showCreateBotVersionRequest :: Show CreateBotVersionRequest where
  show = genericShow
instance decodeCreateBotVersionRequest :: Decode CreateBotVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBotVersionRequest :: Encode CreateBotVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBotVersionResponse = CreateBotVersionResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Intents'" :: NullOrUndefined.NullOrUndefined (IntentList)
  , "ClarificationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "AbortStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Status)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "IdleSessionTTLInSeconds'" :: NullOrUndefined.NullOrUndefined (SessionTTL)
  , "VoiceId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale)
  , "ChildDirected'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCreateBotVersionResponse :: Newtype CreateBotVersionResponse _
derive instance repGenericCreateBotVersionResponse :: Generic CreateBotVersionResponse _
instance showCreateBotVersionResponse :: Show CreateBotVersionResponse where
  show = genericShow
instance decodeCreateBotVersionResponse :: Decode CreateBotVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBotVersionResponse :: Encode CreateBotVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIntentVersionRequest = CreateIntentVersionRequest 
  { "Name'" :: (IntentName)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateIntentVersionRequest :: Newtype CreateIntentVersionRequest _
derive instance repGenericCreateIntentVersionRequest :: Generic CreateIntentVersionRequest _
instance showCreateIntentVersionRequest :: Show CreateIntentVersionRequest where
  show = genericShow
instance decodeCreateIntentVersionRequest :: Decode CreateIntentVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIntentVersionRequest :: Encode CreateIntentVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIntentVersionResponse = CreateIntentVersionResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (IntentName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (SlotList)
  , "SampleUtterances'" :: NullOrUndefined.NullOrUndefined (IntentUtteranceList)
  , "ConfirmationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "RejectionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "FollowUpPrompt'" :: NullOrUndefined.NullOrUndefined (FollowUpPrompt)
  , "ConclusionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "DialogCodeHook'" :: NullOrUndefined.NullOrUndefined (CodeHook)
  , "FulfillmentActivity'" :: NullOrUndefined.NullOrUndefined (FulfillmentActivity)
  , "ParentIntentSignature'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSignature)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateIntentVersionResponse :: Newtype CreateIntentVersionResponse _
derive instance repGenericCreateIntentVersionResponse :: Generic CreateIntentVersionResponse _
instance showCreateIntentVersionResponse :: Show CreateIntentVersionResponse where
  show = genericShow
instance decodeCreateIntentVersionResponse :: Decode CreateIntentVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIntentVersionResponse :: Encode CreateIntentVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSlotTypeVersionRequest = CreateSlotTypeVersionRequest 
  { "Name'" :: (SlotTypeName)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateSlotTypeVersionRequest :: Newtype CreateSlotTypeVersionRequest _
derive instance repGenericCreateSlotTypeVersionRequest :: Generic CreateSlotTypeVersionRequest _
instance showCreateSlotTypeVersionRequest :: Show CreateSlotTypeVersionRequest where
  show = genericShow
instance decodeCreateSlotTypeVersionRequest :: Decode CreateSlotTypeVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSlotTypeVersionRequest :: Encode CreateSlotTypeVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSlotTypeVersionResponse = CreateSlotTypeVersionResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (SlotTypeName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "EnumerationValues'" :: NullOrUndefined.NullOrUndefined (EnumerationValues)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "ValueSelectionStrategy'" :: NullOrUndefined.NullOrUndefined (SlotValueSelectionStrategy)
  }
derive instance newtypeCreateSlotTypeVersionResponse :: Newtype CreateSlotTypeVersionResponse _
derive instance repGenericCreateSlotTypeVersionResponse :: Generic CreateSlotTypeVersionResponse _
instance showCreateSlotTypeVersionResponse :: Show CreateSlotTypeVersionResponse where
  show = genericShow
instance decodeCreateSlotTypeVersionResponse :: Decode CreateSlotTypeVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSlotTypeVersionResponse :: Encode CreateSlotTypeVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomOrBuiltinSlotTypeName = CustomOrBuiltinSlotTypeName String
derive instance newtypeCustomOrBuiltinSlotTypeName :: Newtype CustomOrBuiltinSlotTypeName _
derive instance repGenericCustomOrBuiltinSlotTypeName :: Generic CustomOrBuiltinSlotTypeName _
instance showCustomOrBuiltinSlotTypeName :: Show CustomOrBuiltinSlotTypeName where
  show = genericShow
instance decodeCustomOrBuiltinSlotTypeName :: Decode CustomOrBuiltinSlotTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomOrBuiltinSlotTypeName :: Encode CustomOrBuiltinSlotTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBotAliasRequest = DeleteBotAliasRequest 
  { "Name'" :: (AliasName)
  , "BotName'" :: (BotName)
  }
derive instance newtypeDeleteBotAliasRequest :: Newtype DeleteBotAliasRequest _
derive instance repGenericDeleteBotAliasRequest :: Generic DeleteBotAliasRequest _
instance showDeleteBotAliasRequest :: Show DeleteBotAliasRequest where
  show = genericShow
instance decodeDeleteBotAliasRequest :: Decode DeleteBotAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBotAliasRequest :: Encode DeleteBotAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBotChannelAssociationRequest = DeleteBotChannelAssociationRequest 
  { "Name'" :: (BotChannelName)
  , "BotName'" :: (BotName)
  , "BotAlias'" :: (AliasName)
  }
derive instance newtypeDeleteBotChannelAssociationRequest :: Newtype DeleteBotChannelAssociationRequest _
derive instance repGenericDeleteBotChannelAssociationRequest :: Generic DeleteBotChannelAssociationRequest _
instance showDeleteBotChannelAssociationRequest :: Show DeleteBotChannelAssociationRequest where
  show = genericShow
instance decodeDeleteBotChannelAssociationRequest :: Decode DeleteBotChannelAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBotChannelAssociationRequest :: Encode DeleteBotChannelAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBotRequest = DeleteBotRequest 
  { "Name'" :: (BotName)
  }
derive instance newtypeDeleteBotRequest :: Newtype DeleteBotRequest _
derive instance repGenericDeleteBotRequest :: Generic DeleteBotRequest _
instance showDeleteBotRequest :: Show DeleteBotRequest where
  show = genericShow
instance decodeDeleteBotRequest :: Decode DeleteBotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBotRequest :: Encode DeleteBotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBotVersionRequest = DeleteBotVersionRequest 
  { "Name'" :: (BotName)
  , "Version'" :: (NumericalVersion)
  }
derive instance newtypeDeleteBotVersionRequest :: Newtype DeleteBotVersionRequest _
derive instance repGenericDeleteBotVersionRequest :: Generic DeleteBotVersionRequest _
instance showDeleteBotVersionRequest :: Show DeleteBotVersionRequest where
  show = genericShow
instance decodeDeleteBotVersionRequest :: Decode DeleteBotVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBotVersionRequest :: Encode DeleteBotVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIntentRequest = DeleteIntentRequest 
  { "Name'" :: (IntentName)
  }
derive instance newtypeDeleteIntentRequest :: Newtype DeleteIntentRequest _
derive instance repGenericDeleteIntentRequest :: Generic DeleteIntentRequest _
instance showDeleteIntentRequest :: Show DeleteIntentRequest where
  show = genericShow
instance decodeDeleteIntentRequest :: Decode DeleteIntentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIntentRequest :: Encode DeleteIntentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIntentVersionRequest = DeleteIntentVersionRequest 
  { "Name'" :: (IntentName)
  , "Version'" :: (NumericalVersion)
  }
derive instance newtypeDeleteIntentVersionRequest :: Newtype DeleteIntentVersionRequest _
derive instance repGenericDeleteIntentVersionRequest :: Generic DeleteIntentVersionRequest _
instance showDeleteIntentVersionRequest :: Show DeleteIntentVersionRequest where
  show = genericShow
instance decodeDeleteIntentVersionRequest :: Decode DeleteIntentVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIntentVersionRequest :: Encode DeleteIntentVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSlotTypeRequest = DeleteSlotTypeRequest 
  { "Name'" :: (SlotTypeName)
  }
derive instance newtypeDeleteSlotTypeRequest :: Newtype DeleteSlotTypeRequest _
derive instance repGenericDeleteSlotTypeRequest :: Generic DeleteSlotTypeRequest _
instance showDeleteSlotTypeRequest :: Show DeleteSlotTypeRequest where
  show = genericShow
instance decodeDeleteSlotTypeRequest :: Decode DeleteSlotTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSlotTypeRequest :: Encode DeleteSlotTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSlotTypeVersionRequest = DeleteSlotTypeVersionRequest 
  { "Name'" :: (SlotTypeName)
  , "Version'" :: (NumericalVersion)
  }
derive instance newtypeDeleteSlotTypeVersionRequest :: Newtype DeleteSlotTypeVersionRequest _
derive instance repGenericDeleteSlotTypeVersionRequest :: Generic DeleteSlotTypeVersionRequest _
instance showDeleteSlotTypeVersionRequest :: Show DeleteSlotTypeVersionRequest where
  show = genericShow
instance decodeDeleteSlotTypeVersionRequest :: Decode DeleteSlotTypeVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSlotTypeVersionRequest :: Encode DeleteSlotTypeVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUtterancesRequest = DeleteUtterancesRequest 
  { "BotName'" :: (BotName)
  , "UserId'" :: (UserId)
  }
derive instance newtypeDeleteUtterancesRequest :: Newtype DeleteUtterancesRequest _
derive instance repGenericDeleteUtterancesRequest :: Generic DeleteUtterancesRequest _
instance showDeleteUtterancesRequest :: Show DeleteUtterancesRequest where
  show = genericShow
instance decodeDeleteUtterancesRequest :: Decode DeleteUtterancesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUtterancesRequest :: Encode DeleteUtterancesRequest where
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


-- | <p>Each slot type can have a set of values. Each enumeration value represents a value the slot type can take. </p> <p>For example, a pizza ordering bot could have a slot type that specifies the type of crust that the pizza should have. The slot type could include the values </p> <ul> <li> <p>thick</p> </li> <li> <p>thin</p> </li> <li> <p>stuffed</p> </li> </ul>
newtype EnumerationValue = EnumerationValue 
  { "Value'" :: (Value)
  , "Synonyms'" :: NullOrUndefined.NullOrUndefined (SynonymList)
  }
derive instance newtypeEnumerationValue :: Newtype EnumerationValue _
derive instance repGenericEnumerationValue :: Generic EnumerationValue _
instance showEnumerationValue :: Show EnumerationValue where
  show = genericShow
instance decodeEnumerationValue :: Decode EnumerationValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnumerationValue :: Encode EnumerationValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnumerationValues = EnumerationValues (Array EnumerationValue)
derive instance newtypeEnumerationValues :: Newtype EnumerationValues _
derive instance repGenericEnumerationValues :: Generic EnumerationValues _
instance showEnumerationValues :: Show EnumerationValues where
  show = genericShow
instance decodeEnumerationValues :: Decode EnumerationValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnumerationValues :: Encode EnumerationValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportStatus = ExportStatus String
derive instance newtypeExportStatus :: Newtype ExportStatus _
derive instance repGenericExportStatus :: Generic ExportStatus _
instance showExportStatus :: Show ExportStatus where
  show = genericShow
instance decodeExportStatus :: Decode ExportStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportStatus :: Encode ExportStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportType = ExportType String
derive instance newtypeExportType :: Newtype ExportType _
derive instance repGenericExportType :: Generic ExportType _
instance showExportType :: Show ExportType where
  show = genericShow
instance decodeExportType :: Decode ExportType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportType :: Encode ExportType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A prompt for additional activity after an intent is fulfilled. For example, after the <code>OrderPizza</code> intent is fulfilled, you might prompt the user to find out whether the user wants to order drinks.</p>
newtype FollowUpPrompt = FollowUpPrompt 
  { "Prompt'" :: (Prompt)
  , "RejectionStatement'" :: (Statement)
  }
derive instance newtypeFollowUpPrompt :: Newtype FollowUpPrompt _
derive instance repGenericFollowUpPrompt :: Generic FollowUpPrompt _
instance showFollowUpPrompt :: Show FollowUpPrompt where
  show = genericShow
instance decodeFollowUpPrompt :: Decode FollowUpPrompt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFollowUpPrompt :: Encode FollowUpPrompt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes how the intent is fulfilled after the user provides all of the information required for the intent. You can provide a Lambda function to process the intent, or you can return the intent information to the client application. We recommend that you use a Lambda function so that the relevant logic lives in the Cloud and limit the client-side code primarily to presentation. If you need to update the logic, you only update the Lambda function; you don't need to upgrade your client application. </p> <p>Consider the following examples:</p> <ul> <li> <p>In a pizza ordering application, after the user provides all of the information for placing an order, you use a Lambda function to place an order with a pizzeria. </p> </li> <li> <p>In a gaming application, when a user says "pick up a rock," this information must go back to the client application so that it can perform the operation and update the graphics. In this case, you want Amazon Lex to return the intent data to the client. </p> </li> </ul>
newtype FulfillmentActivity = FulfillmentActivity 
  { "Type'" :: (FulfillmentActivityType)
  , "CodeHook'" :: NullOrUndefined.NullOrUndefined (CodeHook)
  }
derive instance newtypeFulfillmentActivity :: Newtype FulfillmentActivity _
derive instance repGenericFulfillmentActivity :: Generic FulfillmentActivity _
instance showFulfillmentActivity :: Show FulfillmentActivity where
  show = genericShow
instance decodeFulfillmentActivity :: Decode FulfillmentActivity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFulfillmentActivity :: Encode FulfillmentActivity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FulfillmentActivityType = FulfillmentActivityType String
derive instance newtypeFulfillmentActivityType :: Newtype FulfillmentActivityType _
derive instance repGenericFulfillmentActivityType :: Generic FulfillmentActivityType _
instance showFulfillmentActivityType :: Show FulfillmentActivityType where
  show = genericShow
instance decodeFulfillmentActivityType :: Decode FulfillmentActivityType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFulfillmentActivityType :: Encode FulfillmentActivityType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotAliasRequest = GetBotAliasRequest 
  { "Name'" :: (AliasName)
  , "BotName'" :: (BotName)
  }
derive instance newtypeGetBotAliasRequest :: Newtype GetBotAliasRequest _
derive instance repGenericGetBotAliasRequest :: Generic GetBotAliasRequest _
instance showGetBotAliasRequest :: Show GetBotAliasRequest where
  show = genericShow
instance decodeGetBotAliasRequest :: Decode GetBotAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotAliasRequest :: Encode GetBotAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotAliasResponse = GetBotAliasResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (AliasName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "BotVersion'" :: NullOrUndefined.NullOrUndefined (Version)
  , "BotName'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBotAliasResponse :: Newtype GetBotAliasResponse _
derive instance repGenericGetBotAliasResponse :: Generic GetBotAliasResponse _
instance showGetBotAliasResponse :: Show GetBotAliasResponse where
  show = genericShow
instance decodeGetBotAliasResponse :: Decode GetBotAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotAliasResponse :: Encode GetBotAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotAliasesRequest = GetBotAliasesRequest 
  { "BotName'" :: (BotName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NameContains'" :: NullOrUndefined.NullOrUndefined (AliasName)
  }
derive instance newtypeGetBotAliasesRequest :: Newtype GetBotAliasesRequest _
derive instance repGenericGetBotAliasesRequest :: Generic GetBotAliasesRequest _
instance showGetBotAliasesRequest :: Show GetBotAliasesRequest where
  show = genericShow
instance decodeGetBotAliasesRequest :: Decode GetBotAliasesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotAliasesRequest :: Encode GetBotAliasesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotAliasesResponse = GetBotAliasesResponse 
  { "BotAliases" :: NullOrUndefined.NullOrUndefined (BotAliasMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetBotAliasesResponse :: Newtype GetBotAliasesResponse _
derive instance repGenericGetBotAliasesResponse :: Generic GetBotAliasesResponse _
instance showGetBotAliasesResponse :: Show GetBotAliasesResponse where
  show = genericShow
instance decodeGetBotAliasesResponse :: Decode GetBotAliasesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotAliasesResponse :: Encode GetBotAliasesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotChannelAssociationRequest = GetBotChannelAssociationRequest 
  { "Name'" :: (BotChannelName)
  , "BotName'" :: (BotName)
  , "BotAlias'" :: (AliasName)
  }
derive instance newtypeGetBotChannelAssociationRequest :: Newtype GetBotChannelAssociationRequest _
derive instance repGenericGetBotChannelAssociationRequest :: Generic GetBotChannelAssociationRequest _
instance showGetBotChannelAssociationRequest :: Show GetBotChannelAssociationRequest where
  show = genericShow
instance decodeGetBotChannelAssociationRequest :: Decode GetBotChannelAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotChannelAssociationRequest :: Encode GetBotChannelAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotChannelAssociationResponse = GetBotChannelAssociationResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (BotChannelName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "BotAlias'" :: NullOrUndefined.NullOrUndefined (AliasName)
  , "BotName'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Type'" :: NullOrUndefined.NullOrUndefined (ChannelType)
  , "BotConfiguration'" :: NullOrUndefined.NullOrUndefined (ChannelConfigurationMap)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ChannelStatus)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetBotChannelAssociationResponse :: Newtype GetBotChannelAssociationResponse _
derive instance repGenericGetBotChannelAssociationResponse :: Generic GetBotChannelAssociationResponse _
instance showGetBotChannelAssociationResponse :: Show GetBotChannelAssociationResponse where
  show = genericShow
instance decodeGetBotChannelAssociationResponse :: Decode GetBotChannelAssociationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotChannelAssociationResponse :: Encode GetBotChannelAssociationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotChannelAssociationsRequest = GetBotChannelAssociationsRequest 
  { "BotName'" :: (BotName)
  , "BotAlias'" :: (AliasNameOrListAll)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NameContains'" :: NullOrUndefined.NullOrUndefined (BotChannelName)
  }
derive instance newtypeGetBotChannelAssociationsRequest :: Newtype GetBotChannelAssociationsRequest _
derive instance repGenericGetBotChannelAssociationsRequest :: Generic GetBotChannelAssociationsRequest _
instance showGetBotChannelAssociationsRequest :: Show GetBotChannelAssociationsRequest where
  show = genericShow
instance decodeGetBotChannelAssociationsRequest :: Decode GetBotChannelAssociationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotChannelAssociationsRequest :: Encode GetBotChannelAssociationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotChannelAssociationsResponse = GetBotChannelAssociationsResponse 
  { "BotChannelAssociations'" :: NullOrUndefined.NullOrUndefined (BotChannelAssociationList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetBotChannelAssociationsResponse :: Newtype GetBotChannelAssociationsResponse _
derive instance repGenericGetBotChannelAssociationsResponse :: Generic GetBotChannelAssociationsResponse _
instance showGetBotChannelAssociationsResponse :: Show GetBotChannelAssociationsResponse where
  show = genericShow
instance decodeGetBotChannelAssociationsResponse :: Decode GetBotChannelAssociationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotChannelAssociationsResponse :: Encode GetBotChannelAssociationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotRequest = GetBotRequest 
  { "Name'" :: (BotName)
  , "VersionOrAlias'" :: (String)
  }
derive instance newtypeGetBotRequest :: Newtype GetBotRequest _
derive instance repGenericGetBotRequest :: Generic GetBotRequest _
instance showGetBotRequest :: Show GetBotRequest where
  show = genericShow
instance decodeGetBotRequest :: Decode GetBotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotRequest :: Encode GetBotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotResponse = GetBotResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Intents'" :: NullOrUndefined.NullOrUndefined (IntentList)
  , "ClarificationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "AbortStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Status)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "IdleSessionTTLInSeconds'" :: NullOrUndefined.NullOrUndefined (SessionTTL)
  , "VoiceId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale)
  , "ChildDirected'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGetBotResponse :: Newtype GetBotResponse _
derive instance repGenericGetBotResponse :: Generic GetBotResponse _
instance showGetBotResponse :: Show GetBotResponse where
  show = genericShow
instance decodeGetBotResponse :: Decode GetBotResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotResponse :: Encode GetBotResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotVersionsRequest = GetBotVersionsRequest 
  { "Name'" :: (BotName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetBotVersionsRequest :: Newtype GetBotVersionsRequest _
derive instance repGenericGetBotVersionsRequest :: Generic GetBotVersionsRequest _
instance showGetBotVersionsRequest :: Show GetBotVersionsRequest where
  show = genericShow
instance decodeGetBotVersionsRequest :: Decode GetBotVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotVersionsRequest :: Encode GetBotVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotVersionsResponse = GetBotVersionsResponse 
  { "Bots'" :: NullOrUndefined.NullOrUndefined (BotMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetBotVersionsResponse :: Newtype GetBotVersionsResponse _
derive instance repGenericGetBotVersionsResponse :: Generic GetBotVersionsResponse _
instance showGetBotVersionsResponse :: Show GetBotVersionsResponse where
  show = genericShow
instance decodeGetBotVersionsResponse :: Decode GetBotVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotVersionsResponse :: Encode GetBotVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotsRequest = GetBotsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NameContains'" :: NullOrUndefined.NullOrUndefined (BotName)
  }
derive instance newtypeGetBotsRequest :: Newtype GetBotsRequest _
derive instance repGenericGetBotsRequest :: Generic GetBotsRequest _
instance showGetBotsRequest :: Show GetBotsRequest where
  show = genericShow
instance decodeGetBotsRequest :: Decode GetBotsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotsRequest :: Encode GetBotsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBotsResponse = GetBotsResponse 
  { "Bots'" :: NullOrUndefined.NullOrUndefined (BotMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetBotsResponse :: Newtype GetBotsResponse _
derive instance repGenericGetBotsResponse :: Generic GetBotsResponse _
instance showGetBotsResponse :: Show GetBotsResponse where
  show = genericShow
instance decodeGetBotsResponse :: Decode GetBotsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBotsResponse :: Encode GetBotsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBuiltinIntentRequest = GetBuiltinIntentRequest 
  { "Signature'" :: (BuiltinIntentSignature)
  }
derive instance newtypeGetBuiltinIntentRequest :: Newtype GetBuiltinIntentRequest _
derive instance repGenericGetBuiltinIntentRequest :: Generic GetBuiltinIntentRequest _
instance showGetBuiltinIntentRequest :: Show GetBuiltinIntentRequest where
  show = genericShow
instance decodeGetBuiltinIntentRequest :: Decode GetBuiltinIntentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBuiltinIntentRequest :: Encode GetBuiltinIntentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBuiltinIntentResponse = GetBuiltinIntentResponse 
  { "Signature'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSignature)
  , "SupportedLocales'" :: NullOrUndefined.NullOrUndefined (LocaleList)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSlotList)
  }
derive instance newtypeGetBuiltinIntentResponse :: Newtype GetBuiltinIntentResponse _
derive instance repGenericGetBuiltinIntentResponse :: Generic GetBuiltinIntentResponse _
instance showGetBuiltinIntentResponse :: Show GetBuiltinIntentResponse where
  show = genericShow
instance decodeGetBuiltinIntentResponse :: Decode GetBuiltinIntentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBuiltinIntentResponse :: Encode GetBuiltinIntentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBuiltinIntentsRequest = GetBuiltinIntentsRequest 
  { "Locale'" :: NullOrUndefined.NullOrUndefined (Locale)
  , "SignatureContains'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetBuiltinIntentsRequest :: Newtype GetBuiltinIntentsRequest _
derive instance repGenericGetBuiltinIntentsRequest :: Generic GetBuiltinIntentsRequest _
instance showGetBuiltinIntentsRequest :: Show GetBuiltinIntentsRequest where
  show = genericShow
instance decodeGetBuiltinIntentsRequest :: Decode GetBuiltinIntentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBuiltinIntentsRequest :: Encode GetBuiltinIntentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBuiltinIntentsResponse = GetBuiltinIntentsResponse 
  { "Intents'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetBuiltinIntentsResponse :: Newtype GetBuiltinIntentsResponse _
derive instance repGenericGetBuiltinIntentsResponse :: Generic GetBuiltinIntentsResponse _
instance showGetBuiltinIntentsResponse :: Show GetBuiltinIntentsResponse where
  show = genericShow
instance decodeGetBuiltinIntentsResponse :: Decode GetBuiltinIntentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBuiltinIntentsResponse :: Encode GetBuiltinIntentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBuiltinSlotTypesRequest = GetBuiltinSlotTypesRequest 
  { "Locale'" :: NullOrUndefined.NullOrUndefined (Locale)
  , "SignatureContains'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetBuiltinSlotTypesRequest :: Newtype GetBuiltinSlotTypesRequest _
derive instance repGenericGetBuiltinSlotTypesRequest :: Generic GetBuiltinSlotTypesRequest _
instance showGetBuiltinSlotTypesRequest :: Show GetBuiltinSlotTypesRequest where
  show = genericShow
instance decodeGetBuiltinSlotTypesRequest :: Decode GetBuiltinSlotTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBuiltinSlotTypesRequest :: Encode GetBuiltinSlotTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBuiltinSlotTypesResponse = GetBuiltinSlotTypesResponse 
  { "SlotTypes'" :: NullOrUndefined.NullOrUndefined (BuiltinSlotTypeMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetBuiltinSlotTypesResponse :: Newtype GetBuiltinSlotTypesResponse _
derive instance repGenericGetBuiltinSlotTypesResponse :: Generic GetBuiltinSlotTypesResponse _
instance showGetBuiltinSlotTypesResponse :: Show GetBuiltinSlotTypesResponse where
  show = genericShow
instance decodeGetBuiltinSlotTypesResponse :: Decode GetBuiltinSlotTypesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBuiltinSlotTypesResponse :: Encode GetBuiltinSlotTypesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetExportRequest = GetExportRequest 
  { "Name'" :: (Name)
  , "Version'" :: (NumericalVersion)
  , "ResourceType'" :: (ResourceType)
  , "ExportType'" :: (ExportType)
  }
derive instance newtypeGetExportRequest :: Newtype GetExportRequest _
derive instance repGenericGetExportRequest :: Generic GetExportRequest _
instance showGetExportRequest :: Show GetExportRequest where
  show = genericShow
instance decodeGetExportRequest :: Decode GetExportRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetExportRequest :: Encode GetExportRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetExportResponse = GetExportResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Version'" :: NullOrUndefined.NullOrUndefined (NumericalVersion)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "ExportType'" :: NullOrUndefined.NullOrUndefined (ExportType)
  , "ExportStatus'" :: NullOrUndefined.NullOrUndefined (ExportStatus)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "Url'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetExportResponse :: Newtype GetExportResponse _
derive instance repGenericGetExportResponse :: Generic GetExportResponse _
instance showGetExportResponse :: Show GetExportResponse where
  show = genericShow
instance decodeGetExportResponse :: Decode GetExportResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetExportResponse :: Encode GetExportResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetImportRequest = GetImportRequest 
  { "ImportId'" :: (String)
  }
derive instance newtypeGetImportRequest :: Newtype GetImportRequest _
derive instance repGenericGetImportRequest :: Generic GetImportRequest _
instance showGetImportRequest :: Show GetImportRequest where
  show = genericShow
instance decodeGetImportRequest :: Decode GetImportRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetImportRequest :: Encode GetImportRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetImportResponse = GetImportResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "MergeStrategy'" :: NullOrUndefined.NullOrUndefined (MergeStrategy)
  , "ImportId'" :: NullOrUndefined.NullOrUndefined (String)
  , "ImportStatus'" :: NullOrUndefined.NullOrUndefined (ImportStatus)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeGetImportResponse :: Newtype GetImportResponse _
derive instance repGenericGetImportResponse :: Generic GetImportResponse _
instance showGetImportResponse :: Show GetImportResponse where
  show = genericShow
instance decodeGetImportResponse :: Decode GetImportResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetImportResponse :: Encode GetImportResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntentRequest = GetIntentRequest 
  { "Name'" :: (IntentName)
  , "Version'" :: (Version)
  }
derive instance newtypeGetIntentRequest :: Newtype GetIntentRequest _
derive instance repGenericGetIntentRequest :: Generic GetIntentRequest _
instance showGetIntentRequest :: Show GetIntentRequest where
  show = genericShow
instance decodeGetIntentRequest :: Decode GetIntentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntentRequest :: Encode GetIntentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntentResponse = GetIntentResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (IntentName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (SlotList)
  , "SampleUtterances'" :: NullOrUndefined.NullOrUndefined (IntentUtteranceList)
  , "ConfirmationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "RejectionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "FollowUpPrompt'" :: NullOrUndefined.NullOrUndefined (FollowUpPrompt)
  , "ConclusionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "DialogCodeHook'" :: NullOrUndefined.NullOrUndefined (CodeHook)
  , "FulfillmentActivity'" :: NullOrUndefined.NullOrUndefined (FulfillmentActivity)
  , "ParentIntentSignature'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSignature)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetIntentResponse :: Newtype GetIntentResponse _
derive instance repGenericGetIntentResponse :: Generic GetIntentResponse _
instance showGetIntentResponse :: Show GetIntentResponse where
  show = genericShow
instance decodeGetIntentResponse :: Decode GetIntentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntentResponse :: Encode GetIntentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntentVersionsRequest = GetIntentVersionsRequest 
  { "Name'" :: (IntentName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetIntentVersionsRequest :: Newtype GetIntentVersionsRequest _
derive instance repGenericGetIntentVersionsRequest :: Generic GetIntentVersionsRequest _
instance showGetIntentVersionsRequest :: Show GetIntentVersionsRequest where
  show = genericShow
instance decodeGetIntentVersionsRequest :: Decode GetIntentVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntentVersionsRequest :: Encode GetIntentVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntentVersionsResponse = GetIntentVersionsResponse 
  { "Intents'" :: NullOrUndefined.NullOrUndefined (IntentMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetIntentVersionsResponse :: Newtype GetIntentVersionsResponse _
derive instance repGenericGetIntentVersionsResponse :: Generic GetIntentVersionsResponse _
instance showGetIntentVersionsResponse :: Show GetIntentVersionsResponse where
  show = genericShow
instance decodeGetIntentVersionsResponse :: Decode GetIntentVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntentVersionsResponse :: Encode GetIntentVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntentsRequest = GetIntentsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NameContains'" :: NullOrUndefined.NullOrUndefined (IntentName)
  }
derive instance newtypeGetIntentsRequest :: Newtype GetIntentsRequest _
derive instance repGenericGetIntentsRequest :: Generic GetIntentsRequest _
instance showGetIntentsRequest :: Show GetIntentsRequest where
  show = genericShow
instance decodeGetIntentsRequest :: Decode GetIntentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntentsRequest :: Encode GetIntentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIntentsResponse = GetIntentsResponse 
  { "Intents'" :: NullOrUndefined.NullOrUndefined (IntentMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetIntentsResponse :: Newtype GetIntentsResponse _
derive instance repGenericGetIntentsResponse :: Generic GetIntentsResponse _
instance showGetIntentsResponse :: Show GetIntentsResponse where
  show = genericShow
instance decodeGetIntentsResponse :: Decode GetIntentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIntentsResponse :: Encode GetIntentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSlotTypeRequest = GetSlotTypeRequest 
  { "Name'" :: (SlotTypeName)
  , "Version'" :: (Version)
  }
derive instance newtypeGetSlotTypeRequest :: Newtype GetSlotTypeRequest _
derive instance repGenericGetSlotTypeRequest :: Generic GetSlotTypeRequest _
instance showGetSlotTypeRequest :: Show GetSlotTypeRequest where
  show = genericShow
instance decodeGetSlotTypeRequest :: Decode GetSlotTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSlotTypeRequest :: Encode GetSlotTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSlotTypeResponse = GetSlotTypeResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (SlotTypeName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "EnumerationValues'" :: NullOrUndefined.NullOrUndefined (EnumerationValues)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "ValueSelectionStrategy'" :: NullOrUndefined.NullOrUndefined (SlotValueSelectionStrategy)
  }
derive instance newtypeGetSlotTypeResponse :: Newtype GetSlotTypeResponse _
derive instance repGenericGetSlotTypeResponse :: Generic GetSlotTypeResponse _
instance showGetSlotTypeResponse :: Show GetSlotTypeResponse where
  show = genericShow
instance decodeGetSlotTypeResponse :: Decode GetSlotTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSlotTypeResponse :: Encode GetSlotTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSlotTypeVersionsRequest = GetSlotTypeVersionsRequest 
  { "Name'" :: (SlotTypeName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetSlotTypeVersionsRequest :: Newtype GetSlotTypeVersionsRequest _
derive instance repGenericGetSlotTypeVersionsRequest :: Generic GetSlotTypeVersionsRequest _
instance showGetSlotTypeVersionsRequest :: Show GetSlotTypeVersionsRequest where
  show = genericShow
instance decodeGetSlotTypeVersionsRequest :: Decode GetSlotTypeVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSlotTypeVersionsRequest :: Encode GetSlotTypeVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSlotTypeVersionsResponse = GetSlotTypeVersionsResponse 
  { "SlotTypes'" :: NullOrUndefined.NullOrUndefined (SlotTypeMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetSlotTypeVersionsResponse :: Newtype GetSlotTypeVersionsResponse _
derive instance repGenericGetSlotTypeVersionsResponse :: Generic GetSlotTypeVersionsResponse _
instance showGetSlotTypeVersionsResponse :: Show GetSlotTypeVersionsResponse where
  show = genericShow
instance decodeGetSlotTypeVersionsResponse :: Decode GetSlotTypeVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSlotTypeVersionsResponse :: Encode GetSlotTypeVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSlotTypesRequest = GetSlotTypesRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NameContains'" :: NullOrUndefined.NullOrUndefined (SlotTypeName)
  }
derive instance newtypeGetSlotTypesRequest :: Newtype GetSlotTypesRequest _
derive instance repGenericGetSlotTypesRequest :: Generic GetSlotTypesRequest _
instance showGetSlotTypesRequest :: Show GetSlotTypesRequest where
  show = genericShow
instance decodeGetSlotTypesRequest :: Decode GetSlotTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSlotTypesRequest :: Encode GetSlotTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSlotTypesResponse = GetSlotTypesResponse 
  { "SlotTypes'" :: NullOrUndefined.NullOrUndefined (SlotTypeMetadataList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetSlotTypesResponse :: Newtype GetSlotTypesResponse _
derive instance repGenericGetSlotTypesResponse :: Generic GetSlotTypesResponse _
instance showGetSlotTypesResponse :: Show GetSlotTypesResponse where
  show = genericShow
instance decodeGetSlotTypesResponse :: Decode GetSlotTypesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSlotTypesResponse :: Encode GetSlotTypesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUtterancesViewRequest = GetUtterancesViewRequest 
  { "BotName'" :: (BotName)
  , "BotVersions'" :: (BotVersions)
  , "StatusType'" :: (StatusType)
  }
derive instance newtypeGetUtterancesViewRequest :: Newtype GetUtterancesViewRequest _
derive instance repGenericGetUtterancesViewRequest :: Generic GetUtterancesViewRequest _
instance showGetUtterancesViewRequest :: Show GetUtterancesViewRequest where
  show = genericShow
instance decodeGetUtterancesViewRequest :: Decode GetUtterancesViewRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUtterancesViewRequest :: Encode GetUtterancesViewRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUtterancesViewResponse = GetUtterancesViewResponse 
  { "BotName'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "Utterances'" :: NullOrUndefined.NullOrUndefined (ListsOfUtterances)
  }
derive instance newtypeGetUtterancesViewResponse :: Newtype GetUtterancesViewResponse _
derive instance repGenericGetUtterancesViewResponse :: Generic GetUtterancesViewResponse _
instance showGetUtterancesViewResponse :: Show GetUtterancesViewResponse where
  show = genericShow
instance decodeGetUtterancesViewResponse :: Decode GetUtterancesViewResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUtterancesViewResponse :: Encode GetUtterancesViewResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupNumber = GroupNumber Int
derive instance newtypeGroupNumber :: Newtype GroupNumber _
derive instance repGenericGroupNumber :: Generic GroupNumber _
instance showGroupNumber :: Show GroupNumber where
  show = genericShow
instance decodeGroupNumber :: Decode GroupNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupNumber :: Encode GroupNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportStatus = ImportStatus String
derive instance newtypeImportStatus :: Newtype ImportStatus _
derive instance repGenericImportStatus :: Generic ImportStatus _
instance showImportStatus :: Show ImportStatus where
  show = genericShow
instance decodeImportStatus :: Decode ImportStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportStatus :: Encode ImportStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the specific version of an intent.</p>
newtype Intent = Intent 
  { "IntentName'" :: (IntentName)
  , "IntentVersion'" :: (Version)
  }
derive instance newtypeIntent :: Newtype Intent _
derive instance repGenericIntent :: Generic Intent _
instance showIntent :: Show Intent where
  show = genericShow
instance decodeIntent :: Decode Intent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntent :: Encode Intent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntentList = IntentList (Array Intent)
derive instance newtypeIntentList :: Newtype IntentList _
derive instance repGenericIntentList :: Generic IntentList _
instance showIntentList :: Show IntentList where
  show = genericShow
instance decodeIntentList :: Decode IntentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntentList :: Encode IntentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about an intent.</p>
newtype IntentMetadata = IntentMetadata 
  { "Name'" :: NullOrUndefined.NullOrUndefined (IntentName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeIntentMetadata :: Newtype IntentMetadata _
derive instance repGenericIntentMetadata :: Generic IntentMetadata _
instance showIntentMetadata :: Show IntentMetadata where
  show = genericShow
instance decodeIntentMetadata :: Decode IntentMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntentMetadata :: Encode IntentMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntentMetadataList = IntentMetadataList (Array IntentMetadata)
derive instance newtypeIntentMetadataList :: Newtype IntentMetadataList _
derive instance repGenericIntentMetadataList :: Generic IntentMetadataList _
instance showIntentMetadataList :: Show IntentMetadataList where
  show = genericShow
instance decodeIntentMetadataList :: Decode IntentMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntentMetadataList :: Encode IntentMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntentName = IntentName String
derive instance newtypeIntentName :: Newtype IntentName _
derive instance repGenericIntentName :: Generic IntentName _
instance showIntentName :: Show IntentName where
  show = genericShow
instance decodeIntentName :: Decode IntentName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntentName :: Encode IntentName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntentUtteranceList = IntentUtteranceList (Array Utterance)
derive instance newtypeIntentUtteranceList :: Newtype IntentUtteranceList _
derive instance repGenericIntentUtteranceList :: Generic IntentUtteranceList _
instance showIntentUtteranceList :: Show IntentUtteranceList where
  show = genericShow
instance decodeIntentUtteranceList :: Decode IntentUtteranceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntentUtteranceList :: Encode IntentUtteranceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal Amazon Lex error occurred. Try your request again.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _
derive instance repGenericInternalFailureException :: Generic InternalFailureException _
instance showInternalFailureException :: Show InternalFailureException where
  show = genericShow
instance decodeInternalFailureException :: Decode InternalFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalFailureException :: Encode InternalFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LambdaARN = LambdaARN String
derive instance newtypeLambdaARN :: Newtype LambdaARN _
derive instance repGenericLambdaARN :: Generic LambdaARN _
instance showLambdaARN :: Show LambdaARN where
  show = genericShow
instance decodeLambdaARN :: Decode LambdaARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaARN :: Encode LambdaARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request exceeded a limit. Try your request again.</p>
newtype LimitExceededException = LimitExceededException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfUtterance = ListOfUtterance (Array UtteranceData)
derive instance newtypeListOfUtterance :: Newtype ListOfUtterance _
derive instance repGenericListOfUtterance :: Generic ListOfUtterance _
instance showListOfUtterance :: Show ListOfUtterance where
  show = genericShow
instance decodeListOfUtterance :: Decode ListOfUtterance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfUtterance :: Encode ListOfUtterance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListsOfUtterances = ListsOfUtterances (Array UtteranceList)
derive instance newtypeListsOfUtterances :: Newtype ListsOfUtterances _
derive instance repGenericListsOfUtterances :: Generic ListsOfUtterances _
instance showListsOfUtterances :: Show ListsOfUtterances where
  show = genericShow
instance decodeListsOfUtterances :: Decode ListsOfUtterances where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListsOfUtterances :: Encode ListsOfUtterances where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Locale = Locale String
derive instance newtypeLocale :: Newtype Locale _
derive instance repGenericLocale :: Generic Locale _
instance showLocale :: Show Locale where
  show = genericShow
instance decodeLocale :: Decode Locale where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocale :: Encode Locale where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocaleList = LocaleList (Array Locale)
derive instance newtypeLocaleList :: Newtype LocaleList _
derive instance repGenericLocaleList :: Generic LocaleList _
instance showLocaleList :: Show LocaleList where
  show = genericShow
instance decodeLocaleList :: Decode LocaleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocaleList :: Encode LocaleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MergeStrategy = MergeStrategy String
derive instance newtypeMergeStrategy :: Newtype MergeStrategy _
derive instance repGenericMergeStrategy :: Generic MergeStrategy _
instance showMergeStrategy :: Show MergeStrategy where
  show = genericShow
instance decodeMergeStrategy :: Decode MergeStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergeStrategy :: Encode MergeStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The message object that provides the message text and its type.</p>
newtype Message = Message 
  { "ContentType'" :: (ContentType)
  , "Content'" :: (ContentString)
  , "GroupNumber'" :: NullOrUndefined.NullOrUndefined (GroupNumber)
  }
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageList = MessageList (Array Message)
derive instance newtypeMessageList :: Newtype MessageList _
derive instance repGenericMessageList :: Generic MessageList _
instance showMessageList :: Show MessageList where
  show = genericShow
instance decodeMessageList :: Decode MessageList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageList :: Encode MessageList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageVersion = MessageVersion String
derive instance newtypeMessageVersion :: Newtype MessageVersion _
derive instance repGenericMessageVersion :: Generic MessageVersion _
instance showMessageVersion :: Show MessageVersion where
  show = genericShow
instance decodeMessageVersion :: Decode MessageVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageVersion :: Encode MessageVersion where
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


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource specified in the request was not found. Check the resource and try again.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumericalVersion = NumericalVersion String
derive instance newtypeNumericalVersion :: Newtype NumericalVersion _
derive instance repGenericNumericalVersion :: Generic NumericalVersion _
instance showNumericalVersion :: Show NumericalVersion where
  show = genericShow
instance decodeNumericalVersion :: Decode NumericalVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumericalVersion :: Encode NumericalVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The checksum of the resource that you are trying to change does not match the checksum in the request. Check the resource's checksum and try again.</p>
newtype PreconditionFailedException = PreconditionFailedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePreconditionFailedException :: Newtype PreconditionFailedException _
derive instance repGenericPreconditionFailedException :: Generic PreconditionFailedException _
instance showPreconditionFailedException :: Show PreconditionFailedException where
  show = genericShow
instance decodePreconditionFailedException :: Decode PreconditionFailedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreconditionFailedException :: Encode PreconditionFailedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Priority = Priority Int
derive instance newtypePriority :: Newtype Priority _
derive instance repGenericPriority :: Generic Priority _
instance showPriority :: Show Priority where
  show = genericShow
instance decodePriority :: Decode Priority where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePriority :: Encode Priority where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessBehavior = ProcessBehavior String
derive instance newtypeProcessBehavior :: Newtype ProcessBehavior _
derive instance repGenericProcessBehavior :: Generic ProcessBehavior _
instance showProcessBehavior :: Show ProcessBehavior where
  show = genericShow
instance decodeProcessBehavior :: Decode ProcessBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessBehavior :: Encode ProcessBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Obtains information from the user. To define a prompt, provide one or more messages and specify the number of attempts to get information from the user. If you provide more than one message, Amazon Lex chooses one of the messages to use to prompt the user. For more information, see <a>how-it-works</a>.</p>
newtype Prompt = Prompt 
  { "Messages'" :: (MessageList)
  , "MaxAttempts'" :: (PromptMaxAttempts)
  , "ResponseCard'" :: NullOrUndefined.NullOrUndefined (ResponseCard)
  }
derive instance newtypePrompt :: Newtype Prompt _
derive instance repGenericPrompt :: Generic Prompt _
instance showPrompt :: Show Prompt where
  show = genericShow
instance decodePrompt :: Decode Prompt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrompt :: Encode Prompt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PromptMaxAttempts = PromptMaxAttempts Int
derive instance newtypePromptMaxAttempts :: Newtype PromptMaxAttempts _
derive instance repGenericPromptMaxAttempts :: Generic PromptMaxAttempts _
instance showPromptMaxAttempts :: Show PromptMaxAttempts where
  show = genericShow
instance decodePromptMaxAttempts :: Decode PromptMaxAttempts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePromptMaxAttempts :: Encode PromptMaxAttempts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBotAliasRequest = PutBotAliasRequest 
  { "Name'" :: (AliasName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "BotVersion'" :: (Version)
  , "BotName'" :: (BotName)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePutBotAliasRequest :: Newtype PutBotAliasRequest _
derive instance repGenericPutBotAliasRequest :: Generic PutBotAliasRequest _
instance showPutBotAliasRequest :: Show PutBotAliasRequest where
  show = genericShow
instance decodePutBotAliasRequest :: Decode PutBotAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBotAliasRequest :: Encode PutBotAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBotAliasResponse = PutBotAliasResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (AliasName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "BotVersion'" :: NullOrUndefined.NullOrUndefined (Version)
  , "BotName'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePutBotAliasResponse :: Newtype PutBotAliasResponse _
derive instance repGenericPutBotAliasResponse :: Generic PutBotAliasResponse _
instance showPutBotAliasResponse :: Show PutBotAliasResponse where
  show = genericShow
instance decodePutBotAliasResponse :: Decode PutBotAliasResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBotAliasResponse :: Encode PutBotAliasResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBotRequest = PutBotRequest 
  { "Name'" :: (BotName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Intents'" :: NullOrUndefined.NullOrUndefined (IntentList)
  , "ClarificationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "AbortStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "IdleSessionTTLInSeconds'" :: NullOrUndefined.NullOrUndefined (SessionTTL)
  , "VoiceId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "ProcessBehavior'" :: NullOrUndefined.NullOrUndefined (ProcessBehavior)
  , "Locale'" :: (Locale)
  , "ChildDirected'" :: (Boolean)
  , "CreateVersion'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutBotRequest :: Newtype PutBotRequest _
derive instance repGenericPutBotRequest :: Generic PutBotRequest _
instance showPutBotRequest :: Show PutBotRequest where
  show = genericShow
instance decodePutBotRequest :: Decode PutBotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBotRequest :: Encode PutBotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutBotResponse = PutBotResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (BotName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Intents'" :: NullOrUndefined.NullOrUndefined (IntentList)
  , "ClarificationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "AbortStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "Status'" :: NullOrUndefined.NullOrUndefined (Status)
  , "FailureReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "IdleSessionTTLInSeconds'" :: NullOrUndefined.NullOrUndefined (SessionTTL)
  , "VoiceId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale)
  , "ChildDirected'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CreateVersion'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutBotResponse :: Newtype PutBotResponse _
derive instance repGenericPutBotResponse :: Generic PutBotResponse _
instance showPutBotResponse :: Show PutBotResponse where
  show = genericShow
instance decodePutBotResponse :: Decode PutBotResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutBotResponse :: Encode PutBotResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutIntentRequest = PutIntentRequest 
  { "Name'" :: (IntentName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (SlotList)
  , "SampleUtterances'" :: NullOrUndefined.NullOrUndefined (IntentUtteranceList)
  , "ConfirmationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "RejectionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "FollowUpPrompt'" :: NullOrUndefined.NullOrUndefined (FollowUpPrompt)
  , "ConclusionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "DialogCodeHook'" :: NullOrUndefined.NullOrUndefined (CodeHook)
  , "FulfillmentActivity'" :: NullOrUndefined.NullOrUndefined (FulfillmentActivity)
  , "ParentIntentSignature'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSignature)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreateVersion'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutIntentRequest :: Newtype PutIntentRequest _
derive instance repGenericPutIntentRequest :: Generic PutIntentRequest _
instance showPutIntentRequest :: Show PutIntentRequest where
  show = genericShow
instance decodePutIntentRequest :: Decode PutIntentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutIntentRequest :: Encode PutIntentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutIntentResponse = PutIntentResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (IntentName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (SlotList)
  , "SampleUtterances'" :: NullOrUndefined.NullOrUndefined (IntentUtteranceList)
  , "ConfirmationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "RejectionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "FollowUpPrompt'" :: NullOrUndefined.NullOrUndefined (FollowUpPrompt)
  , "ConclusionStatement'" :: NullOrUndefined.NullOrUndefined (Statement)
  , "DialogCodeHook'" :: NullOrUndefined.NullOrUndefined (CodeHook)
  , "FulfillmentActivity'" :: NullOrUndefined.NullOrUndefined (FulfillmentActivity)
  , "ParentIntentSignature'" :: NullOrUndefined.NullOrUndefined (BuiltinIntentSignature)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreateVersion'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutIntentResponse :: Newtype PutIntentResponse _
derive instance repGenericPutIntentResponse :: Generic PutIntentResponse _
instance showPutIntentResponse :: Show PutIntentResponse where
  show = genericShow
instance decodePutIntentResponse :: Decode PutIntentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutIntentResponse :: Encode PutIntentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutSlotTypeRequest = PutSlotTypeRequest 
  { "Name'" :: (SlotTypeName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "EnumerationValues'" :: NullOrUndefined.NullOrUndefined (EnumerationValues)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "ValueSelectionStrategy'" :: NullOrUndefined.NullOrUndefined (SlotValueSelectionStrategy)
  , "CreateVersion'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutSlotTypeRequest :: Newtype PutSlotTypeRequest _
derive instance repGenericPutSlotTypeRequest :: Generic PutSlotTypeRequest _
instance showPutSlotTypeRequest :: Show PutSlotTypeRequest where
  show = genericShow
instance decodePutSlotTypeRequest :: Decode PutSlotTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutSlotTypeRequest :: Encode PutSlotTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutSlotTypeResponse = PutSlotTypeResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (SlotTypeName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "EnumerationValues'" :: NullOrUndefined.NullOrUndefined (EnumerationValues)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Checksum'" :: NullOrUndefined.NullOrUndefined (String)
  , "ValueSelectionStrategy'" :: NullOrUndefined.NullOrUndefined (SlotValueSelectionStrategy)
  , "CreateVersion'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutSlotTypeResponse :: Newtype PutSlotTypeResponse _
derive instance repGenericPutSlotTypeResponse :: Generic PutSlotTypeResponse _
instance showPutSlotTypeResponse :: Show PutSlotTypeResponse where
  show = genericShow
instance decodePutSlotTypeResponse :: Decode PutSlotTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutSlotTypeResponse :: Encode PutSlotTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReferenceType = ReferenceType String
derive instance newtypeReferenceType :: Newtype ReferenceType _
derive instance repGenericReferenceType :: Generic ReferenceType _
instance showReferenceType :: Show ReferenceType where
  show = genericShow
instance decodeReferenceType :: Decode ReferenceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceType :: Encode ReferenceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource that you are attempting to delete is referred to by another resource. Use this information to remove references to the resource that you are trying to delete.</p> <p>The body of the exception contains a JSON object that describes the resource.</p> <p> <code>{ "resourceType": BOT | BOTALIAS | BOTCHANNEL | INTENT,</code> </p> <p> <code>"resourceReference": {</code> </p> <p> <code>"name": <i>string</i>, "version": <i>string</i> } }</code> </p>
newtype ResourceInUseException = ResourceInUseException 
  { "ReferenceType'" :: NullOrUndefined.NullOrUndefined (ReferenceType)
  , "ExampleReference'" :: NullOrUndefined.NullOrUndefined (ResourceReference)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the resource that refers to the resource that you are attempting to delete. This object is returned as part of the <code>ResourceInUseException</code> exception. </p>
newtype ResourceReference = ResourceReference 
  { "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeResourceReference :: Newtype ResourceReference _
derive instance repGenericResourceReference :: Generic ResourceReference _
instance showResourceReference :: Show ResourceReference where
  show = genericShow
instance decodeResourceReference :: Decode ResourceReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceReference :: Encode ResourceReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseCard = ResponseCard String
derive instance newtypeResponseCard :: Newtype ResponseCard _
derive instance repGenericResponseCard :: Generic ResponseCard _
instance showResponseCard :: Show ResponseCard where
  show = genericShow
instance decodeResponseCard :: Decode ResponseCard where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseCard :: Encode ResponseCard where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SessionTTL = SessionTTL Int
derive instance newtypeSessionTTL :: Newtype SessionTTL _
derive instance repGenericSessionTTL :: Generic SessionTTL _
instance showSessionTTL :: Show SessionTTL where
  show = genericShow
instance decodeSessionTTL :: Decode SessionTTL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSessionTTL :: Encode SessionTTL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the version of a specific slot.</p>
newtype Slot = Slot 
  { "Name'" :: (SlotName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "SlotConstraint'" :: (SlotConstraint)
  , "SlotType'" :: NullOrUndefined.NullOrUndefined (CustomOrBuiltinSlotTypeName)
  , "SlotTypeVersion'" :: NullOrUndefined.NullOrUndefined (Version)
  , "ValueElicitationPrompt'" :: NullOrUndefined.NullOrUndefined (Prompt)
  , "Priority'" :: NullOrUndefined.NullOrUndefined (Priority)
  , "SampleUtterances'" :: NullOrUndefined.NullOrUndefined (SlotUtteranceList)
  , "ResponseCard'" :: NullOrUndefined.NullOrUndefined (ResponseCard)
  }
derive instance newtypeSlot :: Newtype Slot _
derive instance repGenericSlot :: Generic Slot _
instance showSlot :: Show Slot where
  show = genericShow
instance decodeSlot :: Decode Slot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlot :: Encode Slot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotConstraint = SlotConstraint String
derive instance newtypeSlotConstraint :: Newtype SlotConstraint _
derive instance repGenericSlotConstraint :: Generic SlotConstraint _
instance showSlotConstraint :: Show SlotConstraint where
  show = genericShow
instance decodeSlotConstraint :: Decode SlotConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotConstraint :: Encode SlotConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotList = SlotList (Array Slot)
derive instance newtypeSlotList :: Newtype SlotList _
derive instance repGenericSlotList :: Generic SlotList _
instance showSlotList :: Show SlotList where
  show = genericShow
instance decodeSlotList :: Decode SlotList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotList :: Encode SlotList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotName = SlotName String
derive instance newtypeSlotName :: Newtype SlotName _
derive instance repGenericSlotName :: Generic SlotName _
instance showSlotName :: Show SlotName where
  show = genericShow
instance decodeSlotName :: Decode SlotName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotName :: Encode SlotName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a slot type..</p>
newtype SlotTypeMetadata = SlotTypeMetadata 
  { "Name'" :: NullOrUndefined.NullOrUndefined (SlotTypeName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeSlotTypeMetadata :: Newtype SlotTypeMetadata _
derive instance repGenericSlotTypeMetadata :: Generic SlotTypeMetadata _
instance showSlotTypeMetadata :: Show SlotTypeMetadata where
  show = genericShow
instance decodeSlotTypeMetadata :: Decode SlotTypeMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotTypeMetadata :: Encode SlotTypeMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotTypeMetadataList = SlotTypeMetadataList (Array SlotTypeMetadata)
derive instance newtypeSlotTypeMetadataList :: Newtype SlotTypeMetadataList _
derive instance repGenericSlotTypeMetadataList :: Generic SlotTypeMetadataList _
instance showSlotTypeMetadataList :: Show SlotTypeMetadataList where
  show = genericShow
instance decodeSlotTypeMetadataList :: Decode SlotTypeMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotTypeMetadataList :: Encode SlotTypeMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotTypeName = SlotTypeName String
derive instance newtypeSlotTypeName :: Newtype SlotTypeName _
derive instance repGenericSlotTypeName :: Generic SlotTypeName _
instance showSlotTypeName :: Show SlotTypeName where
  show = genericShow
instance decodeSlotTypeName :: Decode SlotTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotTypeName :: Encode SlotTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotUtteranceList = SlotUtteranceList (Array Utterance)
derive instance newtypeSlotUtteranceList :: Newtype SlotUtteranceList _
derive instance repGenericSlotUtteranceList :: Generic SlotUtteranceList _
instance showSlotUtteranceList :: Show SlotUtteranceList where
  show = genericShow
instance decodeSlotUtteranceList :: Decode SlotUtteranceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotUtteranceList :: Encode SlotUtteranceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SlotValueSelectionStrategy = SlotValueSelectionStrategy String
derive instance newtypeSlotValueSelectionStrategy :: Newtype SlotValueSelectionStrategy _
derive instance repGenericSlotValueSelectionStrategy :: Generic SlotValueSelectionStrategy _
instance showSlotValueSelectionStrategy :: Show SlotValueSelectionStrategy where
  show = genericShow
instance decodeSlotValueSelectionStrategy :: Decode SlotValueSelectionStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSlotValueSelectionStrategy :: Encode SlotValueSelectionStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartImportRequest = StartImportRequest 
  { "Payload'" :: (String)
  , "ResourceType'" :: (ResourceType)
  , "MergeStrategy'" :: (MergeStrategy)
  }
derive instance newtypeStartImportRequest :: Newtype StartImportRequest _
derive instance repGenericStartImportRequest :: Generic StartImportRequest _
instance showStartImportRequest :: Show StartImportRequest where
  show = genericShow
instance decodeStartImportRequest :: Decode StartImportRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartImportRequest :: Encode StartImportRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartImportResponse = StartImportResponse 
  { "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "MergeStrategy'" :: NullOrUndefined.NullOrUndefined (MergeStrategy)
  , "ImportId'" :: NullOrUndefined.NullOrUndefined (String)
  , "ImportStatus'" :: NullOrUndefined.NullOrUndefined (ImportStatus)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeStartImportResponse :: Newtype StartImportResponse _
derive instance repGenericStartImportResponse :: Generic StartImportResponse _
instance showStartImportResponse :: Show StartImportResponse where
  show = genericShow
instance decodeStartImportResponse :: Decode StartImportResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartImportResponse :: Encode StartImportResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of messages that convey information to the user. At runtime, Amazon Lex selects the message to convey. </p>
newtype Statement = Statement 
  { "Messages'" :: (MessageList)
  , "ResponseCard'" :: NullOrUndefined.NullOrUndefined (ResponseCard)
  }
derive instance newtypeStatement :: Newtype Statement _
derive instance repGenericStatement :: Generic Statement _
instance showStatement :: Show Statement where
  show = genericShow
instance decodeStatement :: Decode Statement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatement :: Encode Statement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _
derive instance repGenericStatus :: Generic Status _
instance showStatus :: Show Status where
  show = genericShow
instance decodeStatus :: Decode Status where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatus :: Encode Status where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusType = StatusType String
derive instance newtypeStatusType :: Newtype StatusType _
derive instance repGenericStatusType :: Generic StatusType _
instance showStatusType :: Show StatusType where
  show = genericShow
instance decodeStatusType :: Decode StatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusType :: Encode StatusType where
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


newtype SynonymList = SynonymList (Array Value)
derive instance newtypeSynonymList :: Newtype SynonymList _
derive instance repGenericSynonymList :: Generic SynonymList _
instance showSynonymList :: Show SynonymList where
  show = genericShow
instance decodeSynonymList :: Decode SynonymList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSynonymList :: Encode SynonymList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserId = UserId String
derive instance newtypeUserId :: Newtype UserId _
derive instance repGenericUserId :: Generic UserId _
instance showUserId :: Show UserId where
  show = genericShow
instance decodeUserId :: Decode UserId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserId :: Encode UserId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Utterance = Utterance String
derive instance newtypeUtterance :: Newtype Utterance _
derive instance repGenericUtterance :: Generic Utterance _
instance showUtterance :: Show Utterance where
  show = genericShow
instance decodeUtterance :: Decode Utterance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtterance :: Encode Utterance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a single utterance that was made to your bot. </p>
newtype UtteranceData = UtteranceData 
  { "UtteranceString'" :: NullOrUndefined.NullOrUndefined (UtteranceString)
  , "Count'" :: NullOrUndefined.NullOrUndefined (Count)
  , "DistinctUsers'" :: NullOrUndefined.NullOrUndefined (Count)
  , "FirstUtteredDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUtteredDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeUtteranceData :: Newtype UtteranceData _
derive instance repGenericUtteranceData :: Generic UtteranceData _
instance showUtteranceData :: Show UtteranceData where
  show = genericShow
instance decodeUtteranceData :: Decode UtteranceData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtteranceData :: Encode UtteranceData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a list of utterances that have been made to a specific version of your bot. The list contains a maximum of 100 utterances.</p>
newtype UtteranceList = UtteranceList 
  { "BotVersion'" :: NullOrUndefined.NullOrUndefined (Version)
  , "Utterances'" :: NullOrUndefined.NullOrUndefined (ListOfUtterance)
  }
derive instance newtypeUtteranceList :: Newtype UtteranceList _
derive instance repGenericUtteranceList :: Generic UtteranceList _
instance showUtteranceList :: Show UtteranceList where
  show = genericShow
instance decodeUtteranceList :: Decode UtteranceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtteranceList :: Encode UtteranceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UtteranceString = UtteranceString String
derive instance newtypeUtteranceString :: Newtype UtteranceString _
derive instance repGenericUtteranceString :: Generic UtteranceString _
instance showUtteranceString :: Show UtteranceString where
  show = genericShow
instance decodeUtteranceString :: Decode UtteranceString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtteranceString :: Encode UtteranceString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance repGenericValue :: Generic Value _
instance showValue :: Show Value where
  show = genericShow
instance decodeValue :: Decode Value where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValue :: Encode Value where
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
