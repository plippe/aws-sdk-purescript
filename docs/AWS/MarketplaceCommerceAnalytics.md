## Module AWS.MarketplaceCommerceAnalytics

Provides AWS Marketplace business intelligence data on-demand.

#### `serviceName`

``` purescript
serviceName :: String
```

#### `generateDataSet`

``` purescript
generateDataSet :: forall eff. GenerateDataSetRequest -> Aff (err :: RequestError | eff) GenerateDataSetResult
```

Given a data set type and data set publication date, asynchronously publishes the requested data set to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.

#### `startSupportDataExport`

``` purescript
startSupportDataExport :: forall eff. StartSupportDataExportRequest -> Aff (err :: RequestError | eff) StartSupportDataExportResult
```

Given a data set type and a from date, asynchronously publishes the requested customer support data to the specified S3 bucket and notifies the specified SNS topic once the data is available. Returns a unique request identifier that can be used to correlate requests with notifications from the SNS topic. Data sets will be published in comma-separated values (CSV) format with the file name {data_set_type}_YYYY-MM-DD'T'HH-mm-ss'Z'.csv. If a file with the same name already exists (e.g. if the same data set is requested twice), the original file will be overwritten by the new file. Requires a Role with an attached permissions policy providing Allow permissions for the following actions: s3:PutObject, s3:GetBucketLocation, sns:GetTopicAttributes, sns:Publish, iam:GetRolePolicy.

#### `CustomerDefinedValues`

``` purescript
newtype CustomerDefinedValues
  = CustomerDefinedValues (Map OptionalKey OptionalValue)
```

#### `DataSetPublicationDate`

``` purescript
newtype DataSetPublicationDate
  = DataSetPublicationDate Number
```

#### `DataSetRequestId`

``` purescript
newtype DataSetRequestId
  = DataSetRequestId String
```

#### `DataSetType`

``` purescript
newtype DataSetType
  = DataSetType String
```

#### `DestinationS3BucketName`

``` purescript
newtype DestinationS3BucketName
  = DestinationS3BucketName String
```

#### `DestinationS3Prefix`

``` purescript
newtype DestinationS3Prefix
  = DestinationS3Prefix String
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `FromDate`

``` purescript
newtype FromDate
  = FromDate Number
```

#### `GenerateDataSetRequest`

``` purescript
newtype GenerateDataSetRequest
  = GenerateDataSetRequest { "DataSetType'" :: DataSetType, "DataSetPublicationDate'" :: DataSetPublicationDate, "RoleNameArn'" :: RoleNameArn, "DestinationS3BucketName'" :: DestinationS3BucketName, "DestinationS3Prefix'" :: NullOrUndefined (DestinationS3Prefix), "SnsTopicArn'" :: SnsTopicArn, "CustomerDefinedValues'" :: NullOrUndefined (CustomerDefinedValues) }
```

Container for the parameters to the GenerateDataSet operation.

#### `GenerateDataSetResult`

``` purescript
newtype GenerateDataSetResult
  = GenerateDataSetResult { "DataSetRequestId'" :: NullOrUndefined (DataSetRequestId) }
```

Container for the result of the GenerateDataSet operation.

#### `MarketplaceCommerceAnalyticsException`

``` purescript
newtype MarketplaceCommerceAnalyticsException
  = MarketplaceCommerceAnalyticsException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

This exception is thrown when an internal service error occurs.

#### `OptionalKey`

``` purescript
newtype OptionalKey
  = OptionalKey String
```

#### `OptionalValue`

``` purescript
newtype OptionalValue
  = OptionalValue String
```

#### `RoleNameArn`

``` purescript
newtype RoleNameArn
  = RoleNameArn String
```

#### `SnsTopicArn`

``` purescript
newtype SnsTopicArn
  = SnsTopicArn String
```

#### `StartSupportDataExportRequest`

``` purescript
newtype StartSupportDataExportRequest
  = StartSupportDataExportRequest { "DataSetType'" :: SupportDataSetType, "FromDate'" :: FromDate, "RoleNameArn'" :: RoleNameArn, "DestinationS3BucketName'" :: DestinationS3BucketName, "DestinationS3Prefix'" :: NullOrUndefined (DestinationS3Prefix), "SnsTopicArn'" :: SnsTopicArn, "CustomerDefinedValues'" :: NullOrUndefined (CustomerDefinedValues) }
```

Container for the parameters to the StartSupportDataExport operation.

#### `StartSupportDataExportResult`

``` purescript
newtype StartSupportDataExportResult
  = StartSupportDataExportResult { "DataSetRequestId'" :: NullOrUndefined (DataSetRequestId) }
```

Container for the result of the StartSupportDataExport operation.

#### `SupportDataSetType`

``` purescript
newtype SupportDataSetType
  = SupportDataSetType String
```


