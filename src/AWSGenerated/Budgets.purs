

-- | <p>Budgets enable you to plan your service usage, service costs, and your RI utilization. You can also track how close your plan is to your budgeted amount or to the free tier limits. Budgets provide you with a quick way to see your usage-to-date and current estimated charges from AWS and to see how much your predicted usage accrues in charges by the end of the month. Budgets also compare current estimates and charges to the amount that you indicated you want to use or spend and lets you see how much of your budget has been used. AWS updates your budget status several times a day. Budgets track your unblended costs, subscriptions, and refunds. You can create the following types of budgets:</p> <ul> <li> <p>Cost budgets allow you to say how much you want to spend on a service.</p> </li> <li> <p>Usage budgets allow you to say how many hours you want to use for one or more services.</p> </li> <li> <p>RI utilization budgets allow you to define a utilization threshold and receive alerts when RIs are tracking below that threshold.</p> </li> </ul> <p>You can create up to 20,000 budgets per AWS master account. Your first two budgets are free of charge. Each additional budget costs $0.02 per day. You can set up optional notifications that warn you if you exceed, or are forecasted to exceed, your budgeted amount. You can have notifications sent to an Amazon SNS topic, to an email address, or to both. For more information, see <a href="https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/budgets-sns-policy.html">Creating an Amazon SNS Topic for Budget Notifications</a>. AWS Free Tier usage alerts via AWS Budgets are provided for you, and do not count toward your budget limits.</p> <p>Service Endpoint</p> <p>The AWS Budgets API provides the following endpoint:</p> <ul> <li> <p>https://budgets.us-east-1.amazonaws.com</p> </li> </ul>
module AWS.Budgets where

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

serviceName = "Budgets" :: String


-- | <p>Creates a budget and, if included, notifications and subscribers. </p>
createBudget :: forall eff. CreateBudgetRequest -> Aff (exception :: EXCEPTION | eff) CreateBudgetResponse
createBudget = Request.request serviceName "createBudget" 


-- | <p>Creates a notification. You must create the budget before you create the associated notification.</p>
createNotification :: forall eff. CreateNotificationRequest -> Aff (exception :: EXCEPTION | eff) CreateNotificationResponse
createNotification = Request.request serviceName "createNotification" 


-- | <p>Creates a subscriber. You must create the associated budget and notification before you create the subscriber.</p>
createSubscriber :: forall eff. CreateSubscriberRequest -> Aff (exception :: EXCEPTION | eff) CreateSubscriberResponse
createSubscriber = Request.request serviceName "createSubscriber" 


-- | <p>Deletes a budget. You can delete your budget at any time.</p> <p> <b>Deleting a budget also deletes the notifications and subscribers associated with that budget.</b> </p>
deleteBudget :: forall eff. DeleteBudgetRequest -> Aff (exception :: EXCEPTION | eff) DeleteBudgetResponse
deleteBudget = Request.request serviceName "deleteBudget" 


-- | <p>Deletes a notification.</p> <p> <b>Deleting a notification also deletes the subscribers associated with the notification.</b> </p>
deleteNotification :: forall eff. DeleteNotificationRequest -> Aff (exception :: EXCEPTION | eff) DeleteNotificationResponse
deleteNotification = Request.request serviceName "deleteNotification" 


-- | <p>Deletes a subscriber.</p> <p> <b>Deleting the last subscriber to a notification also deletes the notification.</b> </p>
deleteSubscriber :: forall eff. DeleteSubscriberRequest -> Aff (exception :: EXCEPTION | eff) DeleteSubscriberResponse
deleteSubscriber = Request.request serviceName "deleteSubscriber" 


-- | <p>Describes a budget.</p>
describeBudget :: forall eff. DescribeBudgetRequest -> Aff (exception :: EXCEPTION | eff) DescribeBudgetResponse
describeBudget = Request.request serviceName "describeBudget" 


-- | <p>Lists the budgets associated with an account.</p>
describeBudgets :: forall eff. DescribeBudgetsRequest -> Aff (exception :: EXCEPTION | eff) DescribeBudgetsResponse
describeBudgets = Request.request serviceName "describeBudgets" 


-- | <p>Lists the notifications associated with a budget.</p>
describeNotificationsForBudget :: forall eff. DescribeNotificationsForBudgetRequest -> Aff (exception :: EXCEPTION | eff) DescribeNotificationsForBudgetResponse
describeNotificationsForBudget = Request.request serviceName "describeNotificationsForBudget" 


-- | <p>Lists the subscribers associated with a notification.</p>
describeSubscribersForNotification :: forall eff. DescribeSubscribersForNotificationRequest -> Aff (exception :: EXCEPTION | eff) DescribeSubscribersForNotificationResponse
describeSubscribersForNotification = Request.request serviceName "describeSubscribersForNotification" 


-- | <p>Updates a budget. You can change every part of a budget except for the <code>budgetName</code> and the <code>calculatedSpend</code>. When a budget is modified, the <code>calculatedSpend</code> drops to zero until AWS has new usage data to use for forecasting.</p>
updateBudget :: forall eff. UpdateBudgetRequest -> Aff (exception :: EXCEPTION | eff) UpdateBudgetResponse
updateBudget = Request.request serviceName "updateBudget" 


-- | <p>Updates a notification.</p>
updateNotification :: forall eff. UpdateNotificationRequest -> Aff (exception :: EXCEPTION | eff) UpdateNotificationResponse
updateNotification = Request.request serviceName "updateNotification" 


-- | <p>Updates a subscriber.</p>
updateSubscriber :: forall eff. UpdateSubscriberRequest -> Aff (exception :: EXCEPTION | eff) UpdateSubscriberResponse
updateSubscriber = Request.request serviceName "updateSubscriber" 


-- | <p>The account ID of the customer. It should be a 12 digit number.</p>
newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of the <code>CreateBudget</code> operation. The content consists of the detailed metadata and data file information, and the current status of the <code>budget</code>.</p> <p>The ARN pattern for a budget is: <code>arn:aws:budgetservice::AccountId:budget/budgetName</code> </p>
newtype Budget = Budget 
  { "BudgetName" :: (BudgetName)
  , "BudgetLimit" :: NullOrUndefined.NullOrUndefined (Spend)
  , "CostFilters" :: NullOrUndefined.NullOrUndefined (CostFilters)
  , "CostTypes" :: NullOrUndefined.NullOrUndefined (CostTypes)
  , "TimeUnit" :: (TimeUnit)
  , "TimePeriod" :: NullOrUndefined.NullOrUndefined (TimePeriod)
  , "CalculatedSpend" :: NullOrUndefined.NullOrUndefined (CalculatedSpend)
  , "BudgetType" :: (BudgetType)
  }
derive instance newtypeBudget :: Newtype Budget _
derive instance repGenericBudget :: Generic Budget _
instance showBudget :: Show Budget where
  show = genericShow
instance decodeBudget :: Decode Budget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBudget :: Encode Budget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A string represents the budget name. No ":" and "\" character is allowed.</p>
newtype BudgetName = BudgetName String
derive instance newtypeBudgetName :: Newtype BudgetName _
derive instance repGenericBudgetName :: Generic BudgetName _
instance showBudgetName :: Show BudgetName where
  show = genericShow
instance decodeBudgetName :: Decode BudgetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBudgetName :: Encode BudgetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The type of a budget. It should be COST, USAGE, or RI_UTILIZATION.</p>
newtype BudgetType = BudgetType String
derive instance newtypeBudgetType :: Newtype BudgetType _
derive instance repGenericBudgetType :: Generic BudgetType _
instance showBudgetType :: Show BudgetType where
  show = genericShow
instance decodeBudgetType :: Decode BudgetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBudgetType :: Encode BudgetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A list of budgets</p>
newtype Budgets = Budgets (Array Budget)
derive instance newtypeBudgets :: Newtype Budgets _
derive instance repGenericBudgets :: Generic Budgets _
instance showBudgets :: Show Budgets where
  show = genericShow
instance decodeBudgets :: Decode Budgets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBudgets :: Encode Budgets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The spend objects associated with this budget. The <code>actualSpend</code> tracks how much you've used, cost, usage, or RI units, and the <code>forecastedSpend</code> tracks how much you are predicted to spend if your current usage remains steady.</p> <p>For example, if it is the 20th of the month and you have spent <code>50</code> dollars on Amazon EC2, your <code>actualSpend</code> is <code>50 USD</code>, and your <code>forecastedSpend</code> is <code>75 USD</code>.</p>
newtype CalculatedSpend = CalculatedSpend 
  { "ActualSpend" :: (Spend)
  , "ForecastedSpend" :: NullOrUndefined.NullOrUndefined (Spend)
  }
derive instance newtypeCalculatedSpend :: Newtype CalculatedSpend _
derive instance repGenericCalculatedSpend :: Generic CalculatedSpend _
instance showCalculatedSpend :: Show CalculatedSpend where
  show = genericShow
instance decodeCalculatedSpend :: Decode CalculatedSpend where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCalculatedSpend :: Encode CalculatedSpend where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The comparison operator of a notification. Currently we support less than, equal to and greater than.</p>
newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _
derive instance repGenericComparisonOperator :: Generic ComparisonOperator _
instance showComparisonOperator :: Show ComparisonOperator where
  show = genericShow
instance decodeComparisonOperator :: Decode ComparisonOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparisonOperator :: Encode ComparisonOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A map that represents the cost filters applied to the budget.</p>
newtype CostFilters = CostFilters (StrMap.StrMap DimensionValues)
derive instance newtypeCostFilters :: Newtype CostFilters _
derive instance repGenericCostFilters :: Generic CostFilters _
instance showCostFilters :: Show CostFilters where
  show = genericShow
instance decodeCostFilters :: Decode CostFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCostFilters :: Encode CostFilters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The types of cost included in a budget, such as tax and subscriptions.</p>
newtype CostTypes = CostTypes 
  { "IncludeTax" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeSubscription" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "UseBlended" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeRefund" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeCredit" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeUpfront" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeRecurring" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeOtherSubscription" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeSupport" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IncludeDiscount" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "UseAmortized" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  }
derive instance newtypeCostTypes :: Newtype CostTypes _
derive instance repGenericCostTypes :: Generic CostTypes _
instance showCostTypes :: Show CostTypes where
  show = genericShow
instance decodeCostTypes :: Decode CostTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCostTypes :: Encode CostTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of CreateBudget </p>
newtype CreateBudgetRequest = CreateBudgetRequest 
  { "AccountId" :: (AccountId)
  , "Budget" :: (Budget)
  , "NotificationsWithSubscribers" :: NullOrUndefined.NullOrUndefined (NotificationWithSubscribersList)
  }
derive instance newtypeCreateBudgetRequest :: Newtype CreateBudgetRequest _
derive instance repGenericCreateBudgetRequest :: Generic CreateBudgetRequest _
instance showCreateBudgetRequest :: Show CreateBudgetRequest where
  show = genericShow
instance decodeCreateBudgetRequest :: Decode CreateBudgetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBudgetRequest :: Encode CreateBudgetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of CreateBudget </p>
newtype CreateBudgetResponse = CreateBudgetResponse Types.NoArguments
derive instance newtypeCreateBudgetResponse :: Newtype CreateBudgetResponse _
derive instance repGenericCreateBudgetResponse :: Generic CreateBudgetResponse _
instance showCreateBudgetResponse :: Show CreateBudgetResponse where
  show = genericShow
instance decodeCreateBudgetResponse :: Decode CreateBudgetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBudgetResponse :: Encode CreateBudgetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of CreateNotification </p>
newtype CreateNotificationRequest = CreateNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "Subscribers" :: (Subscribers)
  }
derive instance newtypeCreateNotificationRequest :: Newtype CreateNotificationRequest _
derive instance repGenericCreateNotificationRequest :: Generic CreateNotificationRequest _
instance showCreateNotificationRequest :: Show CreateNotificationRequest where
  show = genericShow
instance decodeCreateNotificationRequest :: Decode CreateNotificationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNotificationRequest :: Encode CreateNotificationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of CreateNotification </p>
newtype CreateNotificationResponse = CreateNotificationResponse Types.NoArguments
derive instance newtypeCreateNotificationResponse :: Newtype CreateNotificationResponse _
derive instance repGenericCreateNotificationResponse :: Generic CreateNotificationResponse _
instance showCreateNotificationResponse :: Show CreateNotificationResponse where
  show = genericShow
instance decodeCreateNotificationResponse :: Decode CreateNotificationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNotificationResponse :: Encode CreateNotificationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of CreateSubscriber </p>
newtype CreateSubscriberRequest = CreateSubscriberRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "Subscriber" :: (Subscriber)
  }
derive instance newtypeCreateSubscriberRequest :: Newtype CreateSubscriberRequest _
derive instance repGenericCreateSubscriberRequest :: Generic CreateSubscriberRequest _
instance showCreateSubscriberRequest :: Show CreateSubscriberRequest where
  show = genericShow
instance decodeCreateSubscriberRequest :: Decode CreateSubscriberRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSubscriberRequest :: Encode CreateSubscriberRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of CreateSubscriber </p>
newtype CreateSubscriberResponse = CreateSubscriberResponse Types.NoArguments
derive instance newtypeCreateSubscriberResponse :: Newtype CreateSubscriberResponse _
derive instance repGenericCreateSubscriberResponse :: Generic CreateSubscriberResponse _
instance showCreateSubscriberResponse :: Show CreateSubscriberResponse where
  show = genericShow
instance decodeCreateSubscriberResponse :: Decode CreateSubscriberResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSubscriberResponse :: Encode CreateSubscriberResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've exceeded the notification or subscriber limit.</p>
newtype CreationLimitExceededException = CreationLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCreationLimitExceededException :: Newtype CreationLimitExceededException _
derive instance repGenericCreationLimitExceededException :: Generic CreationLimitExceededException _
instance showCreationLimitExceededException :: Show CreationLimitExceededException where
  show = genericShow
instance decodeCreationLimitExceededException :: Decode CreationLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationLimitExceededException :: Encode CreationLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DeleteBudget </p>
newtype DeleteBudgetRequest = DeleteBudgetRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  }
derive instance newtypeDeleteBudgetRequest :: Newtype DeleteBudgetRequest _
derive instance repGenericDeleteBudgetRequest :: Generic DeleteBudgetRequest _
instance showDeleteBudgetRequest :: Show DeleteBudgetRequest where
  show = genericShow
instance decodeDeleteBudgetRequest :: Decode DeleteBudgetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBudgetRequest :: Encode DeleteBudgetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of DeleteBudget </p>
newtype DeleteBudgetResponse = DeleteBudgetResponse Types.NoArguments
derive instance newtypeDeleteBudgetResponse :: Newtype DeleteBudgetResponse _
derive instance repGenericDeleteBudgetResponse :: Generic DeleteBudgetResponse _
instance showDeleteBudgetResponse :: Show DeleteBudgetResponse where
  show = genericShow
instance decodeDeleteBudgetResponse :: Decode DeleteBudgetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBudgetResponse :: Encode DeleteBudgetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DeleteNotification </p>
newtype DeleteNotificationRequest = DeleteNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  }
derive instance newtypeDeleteNotificationRequest :: Newtype DeleteNotificationRequest _
derive instance repGenericDeleteNotificationRequest :: Generic DeleteNotificationRequest _
instance showDeleteNotificationRequest :: Show DeleteNotificationRequest where
  show = genericShow
instance decodeDeleteNotificationRequest :: Decode DeleteNotificationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteNotificationRequest :: Encode DeleteNotificationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of DeleteNotification </p>
newtype DeleteNotificationResponse = DeleteNotificationResponse Types.NoArguments
derive instance newtypeDeleteNotificationResponse :: Newtype DeleteNotificationResponse _
derive instance repGenericDeleteNotificationResponse :: Generic DeleteNotificationResponse _
instance showDeleteNotificationResponse :: Show DeleteNotificationResponse where
  show = genericShow
instance decodeDeleteNotificationResponse :: Decode DeleteNotificationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteNotificationResponse :: Encode DeleteNotificationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DeleteSubscriber </p>
newtype DeleteSubscriberRequest = DeleteSubscriberRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "Subscriber" :: (Subscriber)
  }
derive instance newtypeDeleteSubscriberRequest :: Newtype DeleteSubscriberRequest _
derive instance repGenericDeleteSubscriberRequest :: Generic DeleteSubscriberRequest _
instance showDeleteSubscriberRequest :: Show DeleteSubscriberRequest where
  show = genericShow
instance decodeDeleteSubscriberRequest :: Decode DeleteSubscriberRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSubscriberRequest :: Encode DeleteSubscriberRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of DeleteSubscriber </p>
newtype DeleteSubscriberResponse = DeleteSubscriberResponse Types.NoArguments
derive instance newtypeDeleteSubscriberResponse :: Newtype DeleteSubscriberResponse _
derive instance repGenericDeleteSubscriberResponse :: Generic DeleteSubscriberResponse _
instance showDeleteSubscriberResponse :: Show DeleteSubscriberResponse where
  show = genericShow
instance decodeDeleteSubscriberResponse :: Decode DeleteSubscriberResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSubscriberResponse :: Encode DeleteSubscriberResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DescribeBudget </p>
newtype DescribeBudgetRequest = DescribeBudgetRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  }
derive instance newtypeDescribeBudgetRequest :: Newtype DescribeBudgetRequest _
derive instance repGenericDescribeBudgetRequest :: Generic DescribeBudgetRequest _
instance showDescribeBudgetRequest :: Show DescribeBudgetRequest where
  show = genericShow
instance decodeDescribeBudgetRequest :: Decode DescribeBudgetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBudgetRequest :: Encode DescribeBudgetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of DescribeBudget </p>
newtype DescribeBudgetResponse = DescribeBudgetResponse 
  { "Budget" :: NullOrUndefined.NullOrUndefined (Budget)
  }
derive instance newtypeDescribeBudgetResponse :: Newtype DescribeBudgetResponse _
derive instance repGenericDescribeBudgetResponse :: Generic DescribeBudgetResponse _
instance showDescribeBudgetResponse :: Show DescribeBudgetResponse where
  show = genericShow
instance decodeDescribeBudgetResponse :: Decode DescribeBudgetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBudgetResponse :: Encode DescribeBudgetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DescribeBudgets </p>
newtype DescribeBudgetsRequest = DescribeBudgetsRequest 
  { "AccountId" :: (AccountId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeBudgetsRequest :: Newtype DescribeBudgetsRequest _
derive instance repGenericDescribeBudgetsRequest :: Generic DescribeBudgetsRequest _
instance showDescribeBudgetsRequest :: Show DescribeBudgetsRequest where
  show = genericShow
instance decodeDescribeBudgetsRequest :: Decode DescribeBudgetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBudgetsRequest :: Encode DescribeBudgetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of DescribeBudgets </p>
newtype DescribeBudgetsResponse = DescribeBudgetsResponse 
  { "Budgets" :: NullOrUndefined.NullOrUndefined (Budgets)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeBudgetsResponse :: Newtype DescribeBudgetsResponse _
derive instance repGenericDescribeBudgetsResponse :: Generic DescribeBudgetsResponse _
instance showDescribeBudgetsResponse :: Show DescribeBudgetsResponse where
  show = genericShow
instance decodeDescribeBudgetsResponse :: Decode DescribeBudgetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBudgetsResponse :: Encode DescribeBudgetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DescribeNotificationsForBudget </p>
newtype DescribeNotificationsForBudgetRequest = DescribeNotificationsForBudgetRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeNotificationsForBudgetRequest :: Newtype DescribeNotificationsForBudgetRequest _
derive instance repGenericDescribeNotificationsForBudgetRequest :: Generic DescribeNotificationsForBudgetRequest _
instance showDescribeNotificationsForBudgetRequest :: Show DescribeNotificationsForBudgetRequest where
  show = genericShow
instance decodeDescribeNotificationsForBudgetRequest :: Decode DescribeNotificationsForBudgetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNotificationsForBudgetRequest :: Encode DescribeNotificationsForBudgetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of GetNotificationsForBudget </p>
newtype DescribeNotificationsForBudgetResponse = DescribeNotificationsForBudgetResponse 
  { "Notifications" :: NullOrUndefined.NullOrUndefined (Notifications)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeNotificationsForBudgetResponse :: Newtype DescribeNotificationsForBudgetResponse _
derive instance repGenericDescribeNotificationsForBudgetResponse :: Generic DescribeNotificationsForBudgetResponse _
instance showDescribeNotificationsForBudgetResponse :: Show DescribeNotificationsForBudgetResponse where
  show = genericShow
instance decodeDescribeNotificationsForBudgetResponse :: Decode DescribeNotificationsForBudgetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNotificationsForBudgetResponse :: Encode DescribeNotificationsForBudgetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of DescribeSubscribersForNotification </p>
newtype DescribeSubscribersForNotificationRequest = DescribeSubscribersForNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeSubscribersForNotificationRequest :: Newtype DescribeSubscribersForNotificationRequest _
derive instance repGenericDescribeSubscribersForNotificationRequest :: Generic DescribeSubscribersForNotificationRequest _
instance showDescribeSubscribersForNotificationRequest :: Show DescribeSubscribersForNotificationRequest where
  show = genericShow
instance decodeDescribeSubscribersForNotificationRequest :: Decode DescribeSubscribersForNotificationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSubscribersForNotificationRequest :: Encode DescribeSubscribersForNotificationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of DescribeSubscribersForNotification </p>
newtype DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse 
  { "Subscribers" :: NullOrUndefined.NullOrUndefined (Subscribers)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDescribeSubscribersForNotificationResponse :: Newtype DescribeSubscribersForNotificationResponse _
derive instance repGenericDescribeSubscribersForNotificationResponse :: Generic DescribeSubscribersForNotificationResponse _
instance showDescribeSubscribersForNotificationResponse :: Show DescribeSubscribersForNotificationResponse where
  show = genericShow
instance decodeDescribeSubscribersForNotificationResponse :: Decode DescribeSubscribersForNotificationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSubscribersForNotificationResponse :: Encode DescribeSubscribersForNotificationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionValues = DimensionValues (Array GenericString)
derive instance newtypeDimensionValues :: Newtype DimensionValues _
derive instance repGenericDimensionValues :: Generic DimensionValues _
instance showDimensionValues :: Show DimensionValues where
  show = genericShow
instance decodeDimensionValues :: Decode DimensionValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionValues :: Encode DimensionValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The budget name already exists. Budget names must be unique within an account.</p>
newtype DuplicateRecordException = DuplicateRecordException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeDuplicateRecordException :: Newtype DuplicateRecordException _
derive instance repGenericDuplicateRecordException :: Generic DuplicateRecordException _
instance showDuplicateRecordException :: Show DuplicateRecordException where
  show = genericShow
instance decodeDuplicateRecordException :: Decode DuplicateRecordException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateRecordException :: Encode DuplicateRecordException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pagination token expired.</p>
newtype ExpiredNextTokenException = ExpiredNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeExpiredNextTokenException :: Newtype ExpiredNextTokenException _
derive instance repGenericExpiredNextTokenException :: Generic ExpiredNextTokenException _
instance showExpiredNextTokenException :: Show ExpiredNextTokenException where
  show = genericShow
instance decodeExpiredNextTokenException :: Decode ExpiredNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredNextTokenException :: Encode ExpiredNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A generic String.</p>
newtype GenericString = GenericString String
derive instance newtypeGenericString :: Newtype GenericString _
derive instance repGenericGenericString :: Generic GenericString _
instance showGenericString :: Show GenericString where
  show = genericShow
instance decodeGenericString :: Decode GenericString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericString :: Encode GenericString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A generic timestamp. In Java it is transformed to a Date object.</p>
newtype GenericTimestamp = GenericTimestamp Number
derive instance newtypeGenericTimestamp :: Newtype GenericTimestamp _
derive instance repGenericGenericTimestamp :: Generic GenericTimestamp _
instance showGenericTimestamp :: Show GenericTimestamp where
  show = genericShow
instance decodeGenericTimestamp :: Decode GenericTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericTimestamp :: Encode GenericTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error on the server occurred during the processing of your request. Try again later.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pagination token is invalid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error on the client occurred. Typically, the cause is an invalid input value.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> An integer to represent how many entries a paginated response contains. Maximum is set to 100.</p>
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>We can’t locate the resource that you specified.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A notification associated with a budget. A budget can have up to five notifications. </p> <p>Each notification must have at least one subscriber. A notification can have one SNS subscriber and up to ten email subscribers, for a total of 11 subscribers.</p> <p>For example, if you have a budget for 200 dollars and you want to be notified when you go over 160 dollars, create a notification with the following parameters:</p> <ul> <li> <p>A notificationType of <code>ACTUAL</code> </p> </li> <li> <p>A comparisonOperator of <code>GREATER_THAN</code> </p> </li> <li> <p>A notification threshold of <code>80</code> </p> </li> </ul>
newtype Notification = Notification 
  { "NotificationType" :: (NotificationType)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Threshold" :: (NotificationThreshold)
  , "ThresholdType" :: NullOrUndefined.NullOrUndefined (ThresholdType)
  }
derive instance newtypeNotification :: Newtype Notification _
derive instance repGenericNotification :: Generic Notification _
instance showNotification :: Show Notification where
  show = genericShow
instance decodeNotification :: Decode Notification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotification :: Encode Notification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The threshold of a notification. It should be a number between 0 and 1,000,000,000.</p>
newtype NotificationThreshold = NotificationThreshold Number
derive instance newtypeNotificationThreshold :: Newtype NotificationThreshold _
derive instance repGenericNotificationThreshold :: Generic NotificationThreshold _
instance showNotificationThreshold :: Show NotificationThreshold where
  show = genericShow
instance decodeNotificationThreshold :: Decode NotificationThreshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationThreshold :: Encode NotificationThreshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The type of a notification. It should be ACTUAL or FORECASTED.</p>
newtype NotificationType = NotificationType String
derive instance newtypeNotificationType :: Newtype NotificationType _
derive instance repGenericNotificationType :: Generic NotificationType _
instance showNotificationType :: Show NotificationType where
  show = genericShow
instance decodeNotificationType :: Decode NotificationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationType :: Encode NotificationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A notification with subscribers. A notification can have one SNS subscriber and up to ten email subscribers, for a total of 11 subscribers.</p>
newtype NotificationWithSubscribers = NotificationWithSubscribers 
  { "Notification" :: (Notification)
  , "Subscribers" :: (Subscribers)
  }
derive instance newtypeNotificationWithSubscribers :: Newtype NotificationWithSubscribers _
derive instance repGenericNotificationWithSubscribers :: Generic NotificationWithSubscribers _
instance showNotificationWithSubscribers :: Show NotificationWithSubscribers where
  show = genericShow
instance decodeNotificationWithSubscribers :: Decode NotificationWithSubscribers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationWithSubscribers :: Encode NotificationWithSubscribers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A list of Notifications, each with a list of subscribers.</p>
newtype NotificationWithSubscribersList = NotificationWithSubscribersList (Array NotificationWithSubscribers)
derive instance newtypeNotificationWithSubscribersList :: Newtype NotificationWithSubscribersList _
derive instance repGenericNotificationWithSubscribersList :: Generic NotificationWithSubscribersList _
instance showNotificationWithSubscribersList :: Show NotificationWithSubscribersList where
  show = genericShow
instance decodeNotificationWithSubscribersList :: Decode NotificationWithSubscribersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationWithSubscribersList :: Encode NotificationWithSubscribersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A list of notifications.</p>
newtype Notifications = Notifications (Array Notification)
derive instance newtypeNotifications :: Newtype Notifications _
derive instance repGenericNotifications :: Generic Notifications _
instance showNotifications :: Show Notifications where
  show = genericShow
instance decodeNotifications :: Decode Notifications where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotifications :: Encode Notifications where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableBoolean = NullableBoolean Boolean
derive instance newtypeNullableBoolean :: Newtype NullableBoolean _
derive instance repGenericNullableBoolean :: Generic NullableBoolean _
instance showNullableBoolean :: Show NullableBoolean where
  show = genericShow
instance decodeNullableBoolean :: Decode NullableBoolean where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableBoolean :: Encode NullableBoolean where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A string to represent NumericValue.</p>
newtype NumericValue = NumericValue String
derive instance newtypeNumericValue :: Newtype NumericValue _
derive instance repGenericNumericValue :: Generic NumericValue _
instance showNumericValue :: Show NumericValue where
  show = genericShow
instance decodeNumericValue :: Decode NumericValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumericValue :: Encode NumericValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The amount of cost or usage being measured for a budget.</p> <p>For example, a <code>Spend</code> for <code>3 GB</code> of S3 usage would have the following parameters:</p> <ul> <li> <p>An <code>Amount</code> of <code>3</code> </p> </li> <li> <p>A <code>unit</code> of <code>GB</code> </p> </li> </ul>
newtype Spend = Spend 
  { "Amount" :: (NumericValue)
  , "Unit''" :: (UnitValue)
  }
derive instance newtypeSpend :: Newtype Spend _
derive instance repGenericSpend :: Generic Spend _
instance showSpend :: Show Spend where
  show = genericShow
instance decodeSpend :: Decode Spend where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpend :: Encode Spend where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The subscriber to a budget notification. The subscriber consists of a subscription type and either an Amazon Simple Notification Service topic or an email address.</p> <p>For example, an email subscriber would have the following parameters:</p> <ul> <li> <p>A <code>subscriptionType</code> of <code>EMAIL</code> </p> </li> <li> <p>An <code>address</code> of <code>example@example.com</code> </p> </li> </ul>
newtype Subscriber = Subscriber 
  { "SubscriptionType" :: (SubscriptionType)
  , "Address" :: (SubscriberAddress)
  }
derive instance newtypeSubscriber :: Newtype Subscriber _
derive instance repGenericSubscriber :: Generic Subscriber _
instance showSubscriber :: Show Subscriber where
  show = genericShow
instance decodeSubscriber :: Decode Subscriber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriber :: Encode Subscriber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> String containing email or sns topic for the subscriber address.</p>
newtype SubscriberAddress = SubscriberAddress String
derive instance newtypeSubscriberAddress :: Newtype SubscriberAddress _
derive instance repGenericSubscriberAddress :: Generic SubscriberAddress _
instance showSubscriberAddress :: Show SubscriberAddress where
  show = genericShow
instance decodeSubscriberAddress :: Decode SubscriberAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriberAddress :: Encode SubscriberAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A list of subscribers.</p>
newtype Subscribers = Subscribers (Array Subscriber)
derive instance newtypeSubscribers :: Newtype Subscribers _
derive instance repGenericSubscribers :: Generic Subscribers _
instance showSubscribers :: Show Subscribers where
  show = genericShow
instance decodeSubscribers :: Decode Subscribers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribers :: Encode Subscribers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The subscription type of the subscriber. It can be SMS or EMAIL.</p>
newtype SubscriptionType = SubscriptionType String
derive instance newtypeSubscriptionType :: Newtype SubscriptionType _
derive instance repGenericSubscriptionType :: Generic SubscriptionType _
instance showSubscriptionType :: Show SubscriptionType where
  show = genericShow
instance decodeSubscriptionType :: Decode SubscriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionType :: Encode SubscriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The type of threshold for a notification. It can be PERCENTAGE or ABSOLUTE_VALUE.</p>
newtype ThresholdType = ThresholdType String
derive instance newtypeThresholdType :: Newtype ThresholdType _
derive instance repGenericThresholdType :: Generic ThresholdType _
instance showThresholdType :: Show ThresholdType where
  show = genericShow
instance decodeThresholdType :: Decode ThresholdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThresholdType :: Encode ThresholdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The period of time covered by a budget. Has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date. </p>
newtype TimePeriod = TimePeriod 
  { "Start" :: NullOrUndefined.NullOrUndefined (GenericTimestamp)
  , "End" :: NullOrUndefined.NullOrUndefined (GenericTimestamp)
  }
derive instance newtypeTimePeriod :: Newtype TimePeriod _
derive instance repGenericTimePeriod :: Generic TimePeriod _
instance showTimePeriod :: Show TimePeriod where
  show = genericShow
instance decodeTimePeriod :: Decode TimePeriod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimePeriod :: Encode TimePeriod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The time unit of the budget. e.g. MONTHLY, QUARTERLY, etc.</p>
newtype TimeUnit = TimeUnit String
derive instance newtypeTimeUnit :: Newtype TimeUnit _
derive instance repGenericTimeUnit :: Generic TimeUnit _
instance showTimeUnit :: Show TimeUnit where
  show = genericShow
instance decodeTimeUnit :: Decode TimeUnit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeUnit :: Encode TimeUnit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A string to represent budget spend unit. It should be not null and not empty.</p>
newtype UnitValue = UnitValue String
derive instance newtypeUnitValue :: Newtype UnitValue _
derive instance repGenericUnitValue :: Generic UnitValue _
instance showUnitValue :: Show UnitValue where
  show = genericShow
instance decodeUnitValue :: Decode UnitValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnitValue :: Encode UnitValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of UpdateBudget </p>
newtype UpdateBudgetRequest = UpdateBudgetRequest 
  { "AccountId" :: (AccountId)
  , "NewBudget" :: (Budget)
  }
derive instance newtypeUpdateBudgetRequest :: Newtype UpdateBudgetRequest _
derive instance repGenericUpdateBudgetRequest :: Generic UpdateBudgetRequest _
instance showUpdateBudgetRequest :: Show UpdateBudgetRequest where
  show = genericShow
instance decodeUpdateBudgetRequest :: Decode UpdateBudgetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBudgetRequest :: Encode UpdateBudgetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of UpdateBudget </p>
newtype UpdateBudgetResponse = UpdateBudgetResponse Types.NoArguments
derive instance newtypeUpdateBudgetResponse :: Newtype UpdateBudgetResponse _
derive instance repGenericUpdateBudgetResponse :: Generic UpdateBudgetResponse _
instance showUpdateBudgetResponse :: Show UpdateBudgetResponse where
  show = genericShow
instance decodeUpdateBudgetResponse :: Decode UpdateBudgetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBudgetResponse :: Encode UpdateBudgetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of UpdateNotification </p>
newtype UpdateNotificationRequest = UpdateNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "OldNotification" :: (Notification)
  , "NewNotification" :: (Notification)
  }
derive instance newtypeUpdateNotificationRequest :: Newtype UpdateNotificationRequest _
derive instance repGenericUpdateNotificationRequest :: Generic UpdateNotificationRequest _
instance showUpdateNotificationRequest :: Show UpdateNotificationRequest where
  show = genericShow
instance decodeUpdateNotificationRequest :: Decode UpdateNotificationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNotificationRequest :: Encode UpdateNotificationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of UpdateNotification </p>
newtype UpdateNotificationResponse = UpdateNotificationResponse Types.NoArguments
derive instance newtypeUpdateNotificationResponse :: Newtype UpdateNotificationResponse _
derive instance repGenericUpdateNotificationResponse :: Generic UpdateNotificationResponse _
instance showUpdateNotificationResponse :: Show UpdateNotificationResponse where
  show = genericShow
instance decodeUpdateNotificationResponse :: Decode UpdateNotificationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNotificationResponse :: Encode UpdateNotificationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request of UpdateSubscriber </p>
newtype UpdateSubscriberRequest = UpdateSubscriberRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "OldSubscriber" :: (Subscriber)
  , "NewSubscriber" :: (Subscriber)
  }
derive instance newtypeUpdateSubscriberRequest :: Newtype UpdateSubscriberRequest _
derive instance repGenericUpdateSubscriberRequest :: Generic UpdateSubscriberRequest _
instance showUpdateSubscriberRequest :: Show UpdateSubscriberRequest where
  show = genericShow
instance decodeUpdateSubscriberRequest :: Decode UpdateSubscriberRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSubscriberRequest :: Encode UpdateSubscriberRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Response of UpdateSubscriber </p>
newtype UpdateSubscriberResponse = UpdateSubscriberResponse Types.NoArguments
derive instance newtypeUpdateSubscriberResponse :: Newtype UpdateSubscriberResponse _
derive instance repGenericUpdateSubscriberResponse :: Generic UpdateSubscriberResponse _
instance showUpdateSubscriberResponse :: Show UpdateSubscriberResponse where
  show = genericShow
instance decodeUpdateSubscriberResponse :: Decode UpdateSubscriberResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSubscriberResponse :: Encode UpdateSubscriberResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The error message the exception carries.</p>
newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
derive instance repGenericErrorMessage' :: Generic ErrorMessage' _
instance showErrorMessage' :: Show ErrorMessage' where
  show = genericShow
instance decodeErrorMessage' :: Decode ErrorMessage' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage' :: Encode ErrorMessage' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
