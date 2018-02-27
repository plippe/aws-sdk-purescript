## Module AWS.Budgets

<p>Budgets enable you to plan your service usage, service costs, and your RI utilization. You can also track how close your plan is to your budgeted amount or to the free tier limits. Budgets provide you with a quick way to see your usage-to-date and current estimated charges from AWS and to see how much your predicted usage accrues in charges by the end of the month. Budgets also compare current estimates and charges to the amount that you indicated you want to use or spend and lets you see how much of your budget has been used. AWS updates your budget status several times a day. Budgets track your unblended costs, subscriptions, and refunds. You can create the following types of budgets:</p> <ul> <li> <p>Cost budgets allow you to say how much you want to spend on a service.</p> </li> <li> <p>Usage budgets allow you to say how many hours you want to use for one or more services.</p> </li> <li> <p>RI utilization budgets allow you to define a utilization threshold and receive alerts when RIs are tracking below that threshold.</p> </li> </ul> <p>You can create up to 20,000 budgets per AWS master account. Your first two budgets are free of charge. Each additional budget costs $0.02 per day. You can set up optional notifications that warn you if you exceed, or are forecasted to exceed, your budgeted amount. You can have notifications sent to an Amazon SNS topic, to an email address, or to both. For more information, see <a href="https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/budgets-sns-policy.html">Creating an Amazon SNS Topic for Budget Notifications</a>. AWS Free Tier usage alerts via AWS Budgets are provided for you, and do not count toward your budget limits.</p> <p>Service Endpoint</p> <p>The AWS Budgets API provides the following endpoint:</p> <ul> <li> <p>https://budgets.us-east-1.amazonaws.com</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createBudget`

``` purescript
createBudget :: forall eff. CreateBudgetRequest -> Aff (err :: RequestError | eff) CreateBudgetResponse
```

<p>Creates a budget and, if included, notifications and subscribers. </p>

#### `createNotification`

``` purescript
createNotification :: forall eff. CreateNotificationRequest -> Aff (err :: RequestError | eff) CreateNotificationResponse
```

<p>Creates a notification. You must create the budget before you create the associated notification.</p>

#### `createSubscriber`

``` purescript
createSubscriber :: forall eff. CreateSubscriberRequest -> Aff (err :: RequestError | eff) CreateSubscriberResponse
```

<p>Creates a subscriber. You must create the associated budget and notification before you create the subscriber.</p>

#### `deleteBudget`

``` purescript
deleteBudget :: forall eff. DeleteBudgetRequest -> Aff (err :: RequestError | eff) DeleteBudgetResponse
```

<p>Deletes a budget. You can delete your budget at any time.</p> <p> <b>Deleting a budget also deletes the notifications and subscribers associated with that budget.</b> </p>

#### `deleteNotification`

``` purescript
deleteNotification :: forall eff. DeleteNotificationRequest -> Aff (err :: RequestError | eff) DeleteNotificationResponse
```

<p>Deletes a notification.</p> <p> <b>Deleting a notification also deletes the subscribers associated with the notification.</b> </p>

#### `deleteSubscriber`

``` purescript
deleteSubscriber :: forall eff. DeleteSubscriberRequest -> Aff (err :: RequestError | eff) DeleteSubscriberResponse
```

<p>Deletes a subscriber.</p> <p> <b>Deleting the last subscriber to a notification also deletes the notification.</b> </p>

#### `describeBudget`

``` purescript
describeBudget :: forall eff. DescribeBudgetRequest -> Aff (err :: RequestError | eff) DescribeBudgetResponse
```

<p>Describes a budget.</p>

#### `describeBudgets`

``` purescript
describeBudgets :: forall eff. DescribeBudgetsRequest -> Aff (err :: RequestError | eff) DescribeBudgetsResponse
```

<p>Lists the budgets associated with an account.</p>

#### `describeNotificationsForBudget`

``` purescript
describeNotificationsForBudget :: forall eff. DescribeNotificationsForBudgetRequest -> Aff (err :: RequestError | eff) DescribeNotificationsForBudgetResponse
```

<p>Lists the notifications associated with a budget.</p>

#### `describeSubscribersForNotification`

``` purescript
describeSubscribersForNotification :: forall eff. DescribeSubscribersForNotificationRequest -> Aff (err :: RequestError | eff) DescribeSubscribersForNotificationResponse
```

<p>Lists the subscribers associated with a notification.</p>

#### `updateBudget`

``` purescript
updateBudget :: forall eff. UpdateBudgetRequest -> Aff (err :: RequestError | eff) UpdateBudgetResponse
```

<p>Updates a budget. You can change every part of a budget except for the <code>budgetName</code> and the <code>calculatedSpend</code>. When a budget is modified, the <code>calculatedSpend</code> drops to zero until AWS has new usage data to use for forecasting.</p>

#### `updateNotification`

``` purescript
updateNotification :: forall eff. UpdateNotificationRequest -> Aff (err :: RequestError | eff) UpdateNotificationResponse
```

<p>Updates a notification.</p>

#### `updateSubscriber`

``` purescript
updateSubscriber :: forall eff. UpdateSubscriberRequest -> Aff (err :: RequestError | eff) UpdateSubscriberResponse
```

<p>Updates a subscriber.</p>

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

<p>The account ID of the customer. It should be a 12 digit number.</p>

##### Instances
``` purescript
Newtype AccountId _
```

#### `Budget`

``` purescript
newtype Budget
  = Budget { "BudgetName" :: BudgetName, "BudgetLimit" :: NullOrUndefined (Spend), "CostFilters" :: NullOrUndefined (CostFilters), "CostTypes" :: NullOrUndefined (CostTypes), "TimeUnit" :: TimeUnit, "TimePeriod" :: NullOrUndefined (TimePeriod), "CalculatedSpend" :: NullOrUndefined (CalculatedSpend), "BudgetType" :: BudgetType }
```

<p>Represents the output of the <code>CreateBudget</code> operation. The content consists of the detailed metadata and data file information, and the current status of the <code>budget</code>.</p> <p>The ARN pattern for a budget is: <code>arn:aws:budgetservice::AccountId:budget/budgetName</code> </p>

##### Instances
``` purescript
Newtype Budget _
```

#### `BudgetName`

``` purescript
newtype BudgetName
  = BudgetName String
```

<p> A string represents the budget name. No ":" and "\" character is allowed.</p>

##### Instances
``` purescript
Newtype BudgetName _
```

#### `BudgetType`

``` purescript
newtype BudgetType
  = BudgetType String
```

<p> The type of a budget. It should be COST, USAGE, or RI_UTILIZATION.</p>

##### Instances
``` purescript
Newtype BudgetType _
```

#### `Budgets`

``` purescript
newtype Budgets
  = Budgets (Array Budget)
```

<p> A list of budgets</p>

##### Instances
``` purescript
Newtype Budgets _
```

#### `CalculatedSpend`

``` purescript
newtype CalculatedSpend
  = CalculatedSpend { "ActualSpend" :: Spend, "ForecastedSpend" :: NullOrUndefined (Spend) }
```

<p>The spend objects associated with this budget. The <code>actualSpend</code> tracks how much you've used, cost, usage, or RI units, and the <code>forecastedSpend</code> tracks how much you are predicted to spend if your current usage remains steady.</p> <p>For example, if it is the 20th of the month and you have spent <code>50</code> dollars on Amazon EC2, your <code>actualSpend</code> is <code>50 USD</code>, and your <code>forecastedSpend</code> is <code>75 USD</code>.</p>

##### Instances
``` purescript
Newtype CalculatedSpend _
```

#### `ComparisonOperator`

``` purescript
newtype ComparisonOperator
  = ComparisonOperator String
```

<p> The comparison operator of a notification. Currently we support less than, equal to and greater than.</p>

##### Instances
``` purescript
Newtype ComparisonOperator _
```

#### `CostFilters`

``` purescript
newtype CostFilters
  = CostFilters (Map GenericString DimensionValues)
```

<p> A map that represents the cost filters applied to the budget.</p>

##### Instances
``` purescript
Newtype CostFilters _
```

#### `CostTypes`

``` purescript
newtype CostTypes
  = CostTypes { "IncludeTax" :: NullOrUndefined (NullableBoolean), "IncludeSubscription" :: NullOrUndefined (NullableBoolean), "UseBlended" :: NullOrUndefined (NullableBoolean), "IncludeRefund" :: NullOrUndefined (NullableBoolean), "IncludeCredit" :: NullOrUndefined (NullableBoolean), "IncludeUpfront" :: NullOrUndefined (NullableBoolean), "IncludeRecurring" :: NullOrUndefined (NullableBoolean), "IncludeOtherSubscription" :: NullOrUndefined (NullableBoolean), "IncludeSupport" :: NullOrUndefined (NullableBoolean), "IncludeDiscount" :: NullOrUndefined (NullableBoolean), "UseAmortized" :: NullOrUndefined (NullableBoolean) }
```

<p>The types of cost included in a budget, such as tax and subscriptions.</p>

##### Instances
``` purescript
Newtype CostTypes _
```

#### `CreateBudgetRequest`

``` purescript
newtype CreateBudgetRequest
  = CreateBudgetRequest { "AccountId" :: AccountId, "Budget" :: Budget, "NotificationsWithSubscribers" :: NullOrUndefined (NotificationWithSubscribersList) }
```

<p> Request of CreateBudget </p>

##### Instances
``` purescript
Newtype CreateBudgetRequest _
```

#### `CreateBudgetResponse`

``` purescript
newtype CreateBudgetResponse
  = CreateBudgetResponse {  }
```

<p> Response of CreateBudget </p>

##### Instances
``` purescript
Newtype CreateBudgetResponse _
```

#### `CreateNotificationRequest`

``` purescript
newtype CreateNotificationRequest
  = CreateNotificationRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "Notification" :: Notification, "Subscribers" :: Subscribers }
```

<p> Request of CreateNotification </p>

##### Instances
``` purescript
Newtype CreateNotificationRequest _
```

#### `CreateNotificationResponse`

``` purescript
newtype CreateNotificationResponse
  = CreateNotificationResponse {  }
```

<p> Response of CreateNotification </p>

##### Instances
``` purescript
Newtype CreateNotificationResponse _
```

#### `CreateSubscriberRequest`

``` purescript
newtype CreateSubscriberRequest
  = CreateSubscriberRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "Notification" :: Notification, "Subscriber" :: Subscriber }
```

<p> Request of CreateSubscriber </p>

##### Instances
``` purescript
Newtype CreateSubscriberRequest _
```

#### `CreateSubscriberResponse`

``` purescript
newtype CreateSubscriberResponse
  = CreateSubscriberResponse {  }
```

<p> Response of CreateSubscriber </p>

##### Instances
``` purescript
Newtype CreateSubscriberResponse _
```

#### `CreationLimitExceededException`

``` purescript
newtype CreationLimitExceededException
  = CreationLimitExceededException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>You've exceeded the notification or subscriber limit.</p>

##### Instances
``` purescript
Newtype CreationLimitExceededException _
```

#### `DeleteBudgetRequest`

``` purescript
newtype DeleteBudgetRequest
  = DeleteBudgetRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName }
```

<p> Request of DeleteBudget </p>

##### Instances
``` purescript
Newtype DeleteBudgetRequest _
```

#### `DeleteBudgetResponse`

``` purescript
newtype DeleteBudgetResponse
  = DeleteBudgetResponse {  }
```

<p> Response of DeleteBudget </p>

##### Instances
``` purescript
Newtype DeleteBudgetResponse _
```

#### `DeleteNotificationRequest`

``` purescript
newtype DeleteNotificationRequest
  = DeleteNotificationRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "Notification" :: Notification }
```

<p> Request of DeleteNotification </p>

##### Instances
``` purescript
Newtype DeleteNotificationRequest _
```

#### `DeleteNotificationResponse`

``` purescript
newtype DeleteNotificationResponse
  = DeleteNotificationResponse {  }
```

<p> Response of DeleteNotification </p>

##### Instances
``` purescript
Newtype DeleteNotificationResponse _
```

#### `DeleteSubscriberRequest`

``` purescript
newtype DeleteSubscriberRequest
  = DeleteSubscriberRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "Notification" :: Notification, "Subscriber" :: Subscriber }
```

<p> Request of DeleteSubscriber </p>

##### Instances
``` purescript
Newtype DeleteSubscriberRequest _
```

#### `DeleteSubscriberResponse`

``` purescript
newtype DeleteSubscriberResponse
  = DeleteSubscriberResponse {  }
```

<p> Response of DeleteSubscriber </p>

##### Instances
``` purescript
Newtype DeleteSubscriberResponse _
```

#### `DescribeBudgetRequest`

``` purescript
newtype DescribeBudgetRequest
  = DescribeBudgetRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName }
```

<p> Request of DescribeBudget </p>

##### Instances
``` purescript
Newtype DescribeBudgetRequest _
```

#### `DescribeBudgetResponse`

``` purescript
newtype DescribeBudgetResponse
  = DescribeBudgetResponse { "Budget" :: NullOrUndefined (Budget) }
```

<p> Response of DescribeBudget </p>

##### Instances
``` purescript
Newtype DescribeBudgetResponse _
```

#### `DescribeBudgetsRequest`

``` purescript
newtype DescribeBudgetsRequest
  = DescribeBudgetsRequest { "AccountId" :: AccountId, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (GenericString) }
```

<p> Request of DescribeBudgets </p>

##### Instances
``` purescript
Newtype DescribeBudgetsRequest _
```

#### `DescribeBudgetsResponse`

``` purescript
newtype DescribeBudgetsResponse
  = DescribeBudgetsResponse { "Budgets" :: NullOrUndefined (Budgets), "NextToken" :: NullOrUndefined (GenericString) }
```

<p> Response of DescribeBudgets </p>

##### Instances
``` purescript
Newtype DescribeBudgetsResponse _
```

#### `DescribeNotificationsForBudgetRequest`

``` purescript
newtype DescribeNotificationsForBudgetRequest
  = DescribeNotificationsForBudgetRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (GenericString) }
```

<p> Request of DescribeNotificationsForBudget </p>

##### Instances
``` purescript
Newtype DescribeNotificationsForBudgetRequest _
```

#### `DescribeNotificationsForBudgetResponse`

``` purescript
newtype DescribeNotificationsForBudgetResponse
  = DescribeNotificationsForBudgetResponse { "Notifications" :: NullOrUndefined (Notifications), "NextToken" :: NullOrUndefined (GenericString) }
```

<p> Response of GetNotificationsForBudget </p>

##### Instances
``` purescript
Newtype DescribeNotificationsForBudgetResponse _
```

#### `DescribeSubscribersForNotificationRequest`

``` purescript
newtype DescribeSubscribersForNotificationRequest
  = DescribeSubscribersForNotificationRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "Notification" :: Notification, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (GenericString) }
```

<p> Request of DescribeSubscribersForNotification </p>

##### Instances
``` purescript
Newtype DescribeSubscribersForNotificationRequest _
```

#### `DescribeSubscribersForNotificationResponse`

``` purescript
newtype DescribeSubscribersForNotificationResponse
  = DescribeSubscribersForNotificationResponse { "Subscribers" :: NullOrUndefined (Subscribers), "NextToken" :: NullOrUndefined (GenericString) }
```

<p> Response of DescribeSubscribersForNotification </p>

##### Instances
``` purescript
Newtype DescribeSubscribersForNotificationResponse _
```

#### `DimensionValues`

``` purescript
newtype DimensionValues
  = DimensionValues (Array GenericString)
```

##### Instances
``` purescript
Newtype DimensionValues _
```

#### `DuplicateRecordException`

``` purescript
newtype DuplicateRecordException
  = DuplicateRecordException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The budget name already exists. Budget names must be unique within an account.</p>

##### Instances
``` purescript
Newtype DuplicateRecordException _
```

#### `ExpiredNextTokenException`

``` purescript
newtype ExpiredNextTokenException
  = ExpiredNextTokenException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The pagination token expired.</p>

##### Instances
``` purescript
Newtype ExpiredNextTokenException _
```

#### `GenericString`

``` purescript
newtype GenericString
  = GenericString String
```

<p> A generic String.</p>

##### Instances
``` purescript
Newtype GenericString _
```

#### `GenericTimestamp`

``` purescript
newtype GenericTimestamp
  = GenericTimestamp Number
```

<p> A generic timestamp. In Java it is transformed to a Date object.</p>

##### Instances
``` purescript
Newtype GenericTimestamp _
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>An error on the server occurred during the processing of your request. Try again later.</p>

##### Instances
``` purescript
Newtype InternalErrorException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>The pagination token is invalid.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>An error on the client occurred. Typically, the cause is an invalid input value.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

<p> An integer to represent how many entries a paginated response contains. Maximum is set to 100.</p>

##### Instances
``` purescript
Newtype MaxResults _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (ErrorMessage') }
```

<p>We can’t locate the resource that you specified.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `Notification`

``` purescript
newtype Notification
  = Notification { "NotificationType" :: NotificationType, "ComparisonOperator" :: ComparisonOperator, "Threshold" :: NotificationThreshold, "ThresholdType" :: NullOrUndefined (ThresholdType) }
```

<p>A notification associated with a budget. A budget can have up to five notifications. </p> <p>Each notification must have at least one subscriber. A notification can have one SNS subscriber and up to ten email subscribers, for a total of 11 subscribers.</p> <p>For example, if you have a budget for 200 dollars and you want to be notified when you go over 160 dollars, create a notification with the following parameters:</p> <ul> <li> <p>A notificationType of <code>ACTUAL</code> </p> </li> <li> <p>A comparisonOperator of <code>GREATER_THAN</code> </p> </li> <li> <p>A notification threshold of <code>80</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype Notification _
```

#### `NotificationThreshold`

``` purescript
newtype NotificationThreshold
  = NotificationThreshold Number
```

<p> The threshold of a notification. It should be a number between 0 and 1,000,000,000.</p>

##### Instances
``` purescript
Newtype NotificationThreshold _
```

#### `NotificationType`

``` purescript
newtype NotificationType
  = NotificationType String
```

<p> The type of a notification. It should be ACTUAL or FORECASTED.</p>

##### Instances
``` purescript
Newtype NotificationType _
```

#### `NotificationWithSubscribers`

``` purescript
newtype NotificationWithSubscribers
  = NotificationWithSubscribers { "Notification" :: Notification, "Subscribers" :: Subscribers }
```

<p>A notification with subscribers. A notification can have one SNS subscriber and up to ten email subscribers, for a total of 11 subscribers.</p>

##### Instances
``` purescript
Newtype NotificationWithSubscribers _
```

#### `NotificationWithSubscribersList`

``` purescript
newtype NotificationWithSubscribersList
  = NotificationWithSubscribersList (Array NotificationWithSubscribers)
```

<p> A list of Notifications, each with a list of subscribers.</p>

##### Instances
``` purescript
Newtype NotificationWithSubscribersList _
```

#### `Notifications`

``` purescript
newtype Notifications
  = Notifications (Array Notification)
```

<p> A list of notifications.</p>

##### Instances
``` purescript
Newtype Notifications _
```

#### `NullableBoolean`

``` purescript
newtype NullableBoolean
  = NullableBoolean Boolean
```

##### Instances
``` purescript
Newtype NullableBoolean _
```

#### `NumericValue`

``` purescript
newtype NumericValue
  = NumericValue String
```

<p> A string to represent NumericValue.</p>

##### Instances
``` purescript
Newtype NumericValue _
```

#### `Spend`

``` purescript
newtype Spend
  = Spend { "Amount" :: NumericValue, "Unit''" :: UnitValue }
```

<p>The amount of cost or usage being measured for a budget.</p> <p>For example, a <code>Spend</code> for <code>3 GB</code> of S3 usage would have the following parameters:</p> <ul> <li> <p>An <code>Amount</code> of <code>3</code> </p> </li> <li> <p>A <code>unit</code> of <code>GB</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype Spend _
```

#### `Subscriber`

``` purescript
newtype Subscriber
  = Subscriber { "SubscriptionType" :: SubscriptionType, "Address" :: SubscriberAddress }
```

<p>The subscriber to a budget notification. The subscriber consists of a subscription type and either an Amazon Simple Notification Service topic or an email address.</p> <p>For example, an email subscriber would have the following parameters:</p> <ul> <li> <p>A <code>subscriptionType</code> of <code>EMAIL</code> </p> </li> <li> <p>An <code>address</code> of <code>example@example.com</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype Subscriber _
```

#### `SubscriberAddress`

``` purescript
newtype SubscriberAddress
  = SubscriberAddress String
```

<p> String containing email or sns topic for the subscriber address.</p>

##### Instances
``` purescript
Newtype SubscriberAddress _
```

#### `Subscribers`

``` purescript
newtype Subscribers
  = Subscribers (Array Subscriber)
```

<p> A list of subscribers.</p>

##### Instances
``` purescript
Newtype Subscribers _
```

#### `SubscriptionType`

``` purescript
newtype SubscriptionType
  = SubscriptionType String
```

<p> The subscription type of the subscriber. It can be SMS or EMAIL.</p>

##### Instances
``` purescript
Newtype SubscriptionType _
```

#### `ThresholdType`

``` purescript
newtype ThresholdType
  = ThresholdType String
```

<p> The type of threshold for a notification. It can be PERCENTAGE or ABSOLUTE_VALUE.</p>

##### Instances
``` purescript
Newtype ThresholdType _
```

#### `TimePeriod`

``` purescript
newtype TimePeriod
  = TimePeriod { "Start" :: NullOrUndefined (GenericTimestamp), "End" :: NullOrUndefined (GenericTimestamp) }
```

<p>The period of time covered by a budget. Has a start date and an end date. The start date must come before the end date. There are no restrictions on the end date. </p>

##### Instances
``` purescript
Newtype TimePeriod _
```

#### `TimeUnit`

``` purescript
newtype TimeUnit
  = TimeUnit String
```

<p> The time unit of the budget. e.g. MONTHLY, QUARTERLY, etc.</p>

##### Instances
``` purescript
Newtype TimeUnit _
```

#### `UnitValue`

``` purescript
newtype UnitValue
  = UnitValue String
```

<p> A string to represent budget spend unit. It should be not null and not empty.</p>

##### Instances
``` purescript
Newtype UnitValue _
```

#### `UpdateBudgetRequest`

``` purescript
newtype UpdateBudgetRequest
  = UpdateBudgetRequest { "AccountId" :: AccountId, "NewBudget" :: Budget }
```

<p> Request of UpdateBudget </p>

##### Instances
``` purescript
Newtype UpdateBudgetRequest _
```

#### `UpdateBudgetResponse`

``` purescript
newtype UpdateBudgetResponse
  = UpdateBudgetResponse {  }
```

<p> Response of UpdateBudget </p>

##### Instances
``` purescript
Newtype UpdateBudgetResponse _
```

#### `UpdateNotificationRequest`

``` purescript
newtype UpdateNotificationRequest
  = UpdateNotificationRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "OldNotification" :: Notification, "NewNotification" :: Notification }
```

<p> Request of UpdateNotification </p>

##### Instances
``` purescript
Newtype UpdateNotificationRequest _
```

#### `UpdateNotificationResponse`

``` purescript
newtype UpdateNotificationResponse
  = UpdateNotificationResponse {  }
```

<p> Response of UpdateNotification </p>

##### Instances
``` purescript
Newtype UpdateNotificationResponse _
```

#### `UpdateSubscriberRequest`

``` purescript
newtype UpdateSubscriberRequest
  = UpdateSubscriberRequest { "AccountId" :: AccountId, "BudgetName" :: BudgetName, "Notification" :: Notification, "OldSubscriber" :: Subscriber, "NewSubscriber" :: Subscriber }
```

<p> Request of UpdateSubscriber </p>

##### Instances
``` purescript
Newtype UpdateSubscriberRequest _
```

#### `UpdateSubscriberResponse`

``` purescript
newtype UpdateSubscriberResponse
  = UpdateSubscriberResponse {  }
```

<p> Response of UpdateSubscriber </p>

##### Instances
``` purescript
Newtype UpdateSubscriberResponse _
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

<p>The error message the exception carries.</p>

##### Instances
``` purescript
Newtype ErrorMessage' _
```


