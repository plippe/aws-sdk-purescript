

-- | <p>Budgets enable you to plan your service usage, service costs, and your RI utilization. You can also track how close your plan is to your budgeted amount or to the free tier limits. Budgets provide you with a quick way to see your usage-to-date and current estimated charges from AWS and to see how much your predicted usage accrues in charges by the end of the month. Budgets also compare current estimates and charges to the amount that you indicated you want to use or spend and lets you see how much of your budget has been used. AWS updates your budget status several times a day. Budgets track your unblended costs, subscriptions, and refunds. You can create the following types of budgets:</p> <ul> <li> <p>Cost budgets allow you to say how much you want to spend on a service.</p> </li> <li> <p>Usage budgets allow you to say how many hours you want to use for one or more services.</p> </li> <li> <p>RI utilization budgets allow you to define a utilization threshold and receive alerts when RIs are tracking below that threshold.</p> </li> </ul> <p>You can create up to 20,000 budgets per AWS master account. Your first two budgets are free of charge. Each additional budget costs $0.02 per day. You can set up optional notifications that warn you if you exceed, or are forecasted to exceed, your budgeted amount. You can have notifications sent to an Amazon SNS topic, to an email address, or to both. For more information, see <a href="https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/budgets-sns-policy.html">Creating an Amazon SNS Topic for Budget Notifications</a>. AWS Free Tier usage alerts via AWS Budgets are provided for you, and do not count toward your budget limits.</p> <p>Service Endpoint</p> <p>The AWS Budgets API provides the following endpoint:</p> <ul> <li> <p>https://budgets.us-east-1.amazonaws.com</p> </li> </ul>
module AWS.Budgets where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Budgets" :: String


-- | <p>Creates a budget and, if included, notifications and subscribers. </p>
createBudget :: forall eff. CreateBudgetRequest -> Aff (err :: AWS.RequestError | eff) CreateBudgetResponse
createBudget = AWS.request serviceName "CreateBudget" 


-- | <p>Creates a notification. You must create the budget before you create the associated notification.</p>
createNotification :: forall eff. CreateNotificationRequest -> Aff (err :: AWS.RequestError | eff) CreateNotificationResponse
createNotification = AWS.request serviceName "CreateNotification" 


-- | <p>Creates a subscriber. You must create the associated budget and notification before you create the subscriber.</p>
createSubscriber :: forall eff. CreateSubscriberRequest -> Aff (err :: AWS.RequestError | eff) CreateSubscriberResponse
createSubscriber = AWS.request serviceName "CreateSubscriber" 


-- | <p>Deletes a budget. You can delete your budget at any time.</p> <p> <b>Deleting a budget also deletes the notifications and subscribers associated with that budget.</b> </p>
deleteBudget :: forall eff. DeleteBudgetRequest -> Aff (err :: AWS.RequestError | eff) DeleteBudgetResponse
deleteBudget = AWS.request serviceName "DeleteBudget" 


-- | <p>Deletes a notification.</p> <p> <b>Deleting a notification also deletes the subscribers associated with the notification.</b> </p>
deleteNotification :: forall eff. DeleteNotificationRequest -> Aff (err :: AWS.RequestError | eff) DeleteNotificationResponse
deleteNotification = AWS.request serviceName "DeleteNotification" 


-- | <p>Deletes a subscriber.</p> <p> <b>Deleting the last subscriber to a notification also deletes the notification.</b> </p>
deleteSubscriber :: forall eff. DeleteSubscriberRequest -> Aff (err :: AWS.RequestError | eff) DeleteSubscriberResponse
deleteSubscriber = AWS.request serviceName "DeleteSubscriber" 


-- | <p>Describes a budget.</p>
describeBudget :: forall eff. DescribeBudgetRequest -> Aff (err :: AWS.RequestError | eff) DescribeBudgetResponse
describeBudget = AWS.request serviceName "DescribeBudget" 


-- | <p>Lists the budgets associated with an account.</p>
describeBudgets :: forall eff. DescribeBudgetsRequest -> Aff (err :: AWS.RequestError | eff) DescribeBudgetsResponse
describeBudgets = AWS.request serviceName "DescribeBudgets" 


-- | <p>Lists the notifications associated with a budget.</p>
describeNotificationsForBudget :: forall eff. DescribeNotificationsForBudgetRequest -> Aff (err :: AWS.RequestError | eff) DescribeNotificationsForBudgetResponse
describeNotificationsForBudget = AWS.request serviceName "DescribeNotificationsForBudget" 


-- | <p>Lists the subscribers associated with a notification.</p>
describeSubscribersForNotification :: forall eff. DescribeSubscribersForNotificationRequest -> Aff (err :: AWS.RequestError | eff) DescribeSubscribersForNotificationResponse
describeSubscribersForNotification = AWS.request serviceName "DescribeSubscribersForNotification" 


-- | <p>Updates a budget. You can change every part of a budget except for the <code>budgetName</code> and the <code>calculatedSpend</code>. When a budget is modified, the <code>calculatedSpend</code> drops to zero until AWS has new usage data to use for forecasting.</p>
updateBudget :: forall eff. UpdateBudgetRequest -> Aff (err :: AWS.RequestError | eff) UpdateBudgetResponse
updateBudget = AWS.request serviceName "UpdateBudget" 


-- | <p>Updates a notification.</p>
updateNotification :: forall eff. UpdateNotificationRequest -> Aff (err :: AWS.RequestError | eff) UpdateNotificationResponse
updateNotification = AWS.request serviceName "UpdateNotification" 


-- | <p>Updates a subscriber.</p>
updateSubscriber :: forall eff. UpdateSubscriberRequest -> Aff (err :: AWS.RequestError | eff) UpdateSubscriberResponse
updateSubscriber = AWS.request serviceName "UpdateSubscriber" 


-- | <p>The account ID of the customer. It should be a 12 digit number.</p>
newtype AccountId = AccountId String


-- | <p>Represents the output of the <code>CreateBudget</code> operation. The content consists of the detailed metadata and data file information, and the current status of the <code>budget</code>.</p> <p>The ARN pattern for a budget is: <code>arn:aws:budgetservice::AccountId:budget/budgetName</code> </p>
newtype Budget = Budget 
  { "BudgetName" :: (BudgetName)
  , "BudgetLimit" :: NullOrUndefined (Spend)
  , "CostFilters" :: NullOrUndefined (CostFilters)
  , "CostTypes" :: NullOrUndefined (CostTypes)
  , "TimeUnit" :: (TimeUnit)
  , "TimePeriod" :: NullOrUndefined (TimePeriod)
  , "CalculatedSpend" :: NullOrUndefined (CalculatedSpend)
  , "BudgetType" :: (BudgetType)
  }


-- | <p> A string represents the budget name. No ":" and "\" character is allowed.</p>
newtype BudgetName = BudgetName String


-- | <p> The type of a budget. It should be COST, USAGE, or RI_UTILIZATION.</p>
newtype BudgetType = BudgetType String


-- | <p> A list of budgets</p>
newtype Budgets = Budgets (Array Budget)


-- | <p>The spend objects associated with this budget. The <code>actualSpend</code> tracks how much you've used, cost, usage, or RI units, and the <code>forecastedSpend</code> tracks how much you are predicted to spend if your current usage remains steady.</p> <p>For example, if it is the 20th of the month and you have spent <code>50</code> dollars on Amazon EC2, your <code>actualSpend</code> is <code>50 USD</code>, and your <code>forecastedSpend</code> is <code>75 USD</code>.</p>
newtype CalculatedSpend = CalculatedSpend 
  { "ActualSpend" :: (Spend)
  , "ForecastedSpend" :: NullOrUndefined (Spend)
  }


-- | <p> The comparison operator of a notification. Currently we support less than, equal to and greater than.</p>
newtype ComparisonOperator = ComparisonOperator String


-- | <p> A map that represents the cost filters applied to the budget.</p>
newtype CostFilters = CostFilters (Map GenericString DimensionValues)


-- | <p>The types of cost included in a budget, such as tax and subscriptions.</p>
newtype CostTypes = CostTypes 
  { "IncludeTax" :: NullOrUndefined (NullableBoolean)
  , "IncludeSubscription" :: NullOrUndefined (NullableBoolean)
  , "UseBlended" :: NullOrUndefined (NullableBoolean)
  , "IncludeRefund" :: NullOrUndefined (NullableBoolean)
  , "IncludeCredit" :: NullOrUndefined (NullableBoolean)
  , "IncludeUpfront" :: NullOrUndefined (NullableBoolean)
  , "IncludeRecurring" :: NullOrUndefined (NullableBoolean)
  , "IncludeOtherSubscription" :: NullOrUndefined (NullableBoolean)
  , "IncludeSupport" :: NullOrUndefined (NullableBoolean)
  , "IncludeDiscount" :: NullOrUndefined (NullableBoolean)
  , "UseAmortized" :: NullOrUndefined (NullableBoolean)
  }


-- | <p> Request of CreateBudget </p>
newtype CreateBudgetRequest = CreateBudgetRequest 
  { "AccountId" :: (AccountId)
  , "Budget" :: (Budget)
  , "NotificationsWithSubscribers" :: NullOrUndefined (NotificationWithSubscribersList)
  }


-- | <p> Response of CreateBudget </p>
newtype CreateBudgetResponse = CreateBudgetResponse 
  { 
  }


-- | <p> Request of CreateNotification </p>
newtype CreateNotificationRequest = CreateNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "Subscribers" :: (Subscribers)
  }


-- | <p> Response of CreateNotification </p>
newtype CreateNotificationResponse = CreateNotificationResponse 
  { 
  }


-- | <p> Request of CreateSubscriber </p>
newtype CreateSubscriberRequest = CreateSubscriberRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "Subscriber" :: (Subscriber)
  }


-- | <p> Response of CreateSubscriber </p>
newtype CreateSubscriberResponse = CreateSubscriberResponse 
  { 
  }


-- | <p>You've exceeded the notification or subscriber limit.</p>
newtype CreationLimitExceededException = CreationLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p> Request of DeleteBudget </p>
newtype DeleteBudgetRequest = DeleteBudgetRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  }


-- | <p> Response of DeleteBudget </p>
newtype DeleteBudgetResponse = DeleteBudgetResponse 
  { 
  }


-- | <p> Request of DeleteNotification </p>
newtype DeleteNotificationRequest = DeleteNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  }


-- | <p> Response of DeleteNotification </p>
newtype DeleteNotificationResponse = DeleteNotificationResponse 
  { 
  }


-- | <p> Request of DeleteSubscriber </p>
newtype DeleteSubscriberRequest = DeleteSubscriberRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "Subscriber" :: (Subscriber)
  }


-- | <p> Response of DeleteSubscriber </p>
newtype DeleteSubscriberResponse = DeleteSubscriberResponse 
  { 
  }


-- | <p> Request of DescribeBudget </p>
newtype DescribeBudgetRequest = DescribeBudgetRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  }


-- | <p> Response of DescribeBudget </p>
newtype DescribeBudgetResponse = DescribeBudgetResponse 
  { "Budget" :: NullOrUndefined (Budget)
  }


-- | <p> Request of DescribeBudgets </p>
newtype DescribeBudgetsRequest = DescribeBudgetsRequest 
  { "AccountId" :: (AccountId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | <p> Response of DescribeBudgets </p>
newtype DescribeBudgetsResponse = DescribeBudgetsResponse 
  { "Budgets" :: NullOrUndefined (Budgets)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | <p> Request of DescribeNotificationsForBudget </p>
newtype DescribeNotificationsForBudgetRequest = DescribeNotificationsForBudgetRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | <p> Response of GetNotificationsForBudget </p>
newtype DescribeNotificationsForBudgetResponse = DescribeNotificationsForBudgetResponse 
  { "Notifications" :: NullOrUndefined (Notifications)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | <p> Request of DescribeSubscribersForNotification </p>
newtype DescribeSubscribersForNotificationRequest = DescribeSubscribersForNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


-- | <p> Response of DescribeSubscribersForNotification </p>
newtype DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse 
  { "Subscribers" :: NullOrUndefined (Subscribers)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


newtype DimensionValues = DimensionValues (Array GenericString)


-- | <p>The budget name already exists. Budget names must be unique within an account.</p>
newtype DuplicateRecordException = DuplicateRecordException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The pagination token expired.</p>
newtype ExpiredNextTokenException = ExpiredNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p> A generic String.</p>
newtype GenericString = GenericString String


-- | <p> A generic timestamp. In Java it is transformed to a Date object.</p>
newtype GenericTimestamp = GenericTimestamp Number


-- | <p>An error on the server occurred during the processing of your request. Try again later.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The pagination token is invalid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>An error on the client occurred. Typically, the cause is an invalid input value.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p> An integer to represent how many entries a paginated response contains. Maximum is set to 100.</p>
newtype MaxResults = MaxResults Int


-- | <p>We can’t locate the resource that you specified.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>A notification associated with a budget. A budget can have up to five notifications. </p> <p>Each notification must have at least one subscriber. A notification can have one SNS subscriber and up to ten email subscribers, for a total of 11 subscribers.</p> <p>For example, if you have a budget for 200 dollars and you want to be notified when you go over 160 dollars, create a notification with the following parameters:</p> <ul> <li> <p>A notificationType of <code>ACTUAL</code> </p> </li> <li> <p>A comparisonOperator of <code>GREATER_THAN</code> </p> </li> <li> <p>A notification threshold of <code>80</code> </p> </li> </ul>
newtype Notification = Notification 
  { "NotificationType" :: (NotificationType)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Threshold" :: (NotificationThreshold)
  , "ThresholdType" :: NullOrUndefined (ThresholdType)
  }


-- | <p> The threshold of a notification. It should be a number between 0 and 1,000,000,000.</p>
newtype NotificationThreshold = NotificationThreshold Number


-- | <p> The type of a notification. It should be ACTUAL or FORECASTED.</p>
newtype NotificationType = NotificationType String


-- | <p>A notification with subscribers. A notification can have one SNS subscriber and up to ten email subscribers, for a total of 11 subscribers.</p>
newtype NotificationWithSubscribers = NotificationWithSubscribers 
  { "Notification" :: (Notification)
  , "Subscribers" :: (Subscribers)
  }


-- | <p> A list of Notifications, each with a list of subscribers.</p>
newtype NotificationWithSubscribersList = NotificationWithSubscribersList (Array NotificationWithSubscribers)


-- | <p> A list of notifications.</p>
newtype Notifications = Notifications (Array Notification)


newtype NullableBoolean = NullableBoolean Boolean


-- | <p> A string to represent NumericValue.</p>
newtype NumericValue = NumericValue String


-- | <p>The amount of cost or usage being measured for a budget.</p> <p>For example, a <code>Spend</code> for <code>3 GB</code> of S3 usage would have the following parameters:</p> <ul> <li> <p>An <code>Amount</code> of <code>3</code> </p> </li> <li> <p>A <code>unit</code> of <code>GB</code> </p> </li> </ul>
newtype Spend = Spend 
  { "Amount" :: (NumericValue)
  , "Unit''" :: (UnitValue)
  }


-- | <p>The subscriber to a budget notification. The subscriber consists of a subscription type and either an Amazon Simple Notification Service topic or an email address.</p> <p>For example, an email subscriber would have the following parameters:</p> <ul> <li> <p>A <code>subscriptionType</code> of <code>EMAIL</code> </p> </li> <li> <p>An <code>address</code> of <code>example@example.com</code> </p> </li> </ul>
newtype Subscriber = Subscriber 
  { "SubscriptionType" :: (SubscriptionType)
  , "Address" :: (SubscriberAddress)
  }


-- | <p> String containing email or sns topic for the subscriber address.</p>
newtype SubscriberAddress = SubscriberAddress String


-- | <p> A list of subscribers.</p>
newtype Subscribers = Subscribers (Array Subscriber)


-- | <p> The subscription type of the subscriber. It can be SMS or EMAIL.</p>
newtype SubscriptionType = SubscriptionType String


-- | <p> The type of threshold for a notification. It can be PERCENTAGE or ABSOLUTE_VALUE.</p>
newtype ThresholdType = ThresholdType String


-- | <p>The period of time covered by a budget. Has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date. </p>
newtype TimePeriod = TimePeriod 
  { "Start" :: NullOrUndefined (GenericTimestamp)
  , "End" :: NullOrUndefined (GenericTimestamp)
  }


-- | <p> The time unit of the budget. e.g. MONTHLY, QUARTERLY, etc.</p>
newtype TimeUnit = TimeUnit String


-- | <p> A string to represent budget spend unit. It should be not null and not empty.</p>
newtype UnitValue = UnitValue String


-- | <p> Request of UpdateBudget </p>
newtype UpdateBudgetRequest = UpdateBudgetRequest 
  { "AccountId" :: (AccountId)
  , "NewBudget" :: (Budget)
  }


-- | <p> Response of UpdateBudget </p>
newtype UpdateBudgetResponse = UpdateBudgetResponse 
  { 
  }


-- | <p> Request of UpdateNotification </p>
newtype UpdateNotificationRequest = UpdateNotificationRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "OldNotification" :: (Notification)
  , "NewNotification" :: (Notification)
  }


-- | <p> Response of UpdateNotification </p>
newtype UpdateNotificationResponse = UpdateNotificationResponse 
  { 
  }


-- | <p> Request of UpdateSubscriber </p>
newtype UpdateSubscriberRequest = UpdateSubscriberRequest 
  { "AccountId" :: (AccountId)
  , "BudgetName" :: (BudgetName)
  , "Notification" :: (Notification)
  , "OldSubscriber" :: (Subscriber)
  , "NewSubscriber" :: (Subscriber)
  }


-- | <p> Response of UpdateSubscriber </p>
newtype UpdateSubscriberResponse = UpdateSubscriberResponse 
  { 
  }


-- | <p>The error message the exception carries.</p>
newtype ErrorMessage' = ErrorMessage' String
