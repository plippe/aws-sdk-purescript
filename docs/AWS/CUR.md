## Module AWS.CUR

All public APIs for AWS Cost and Usage Report service

#### `serviceName`

``` purescript
serviceName :: String
```

#### `deleteReportDefinition`

``` purescript
deleteReportDefinition :: forall eff. DeleteReportDefinitionRequest -> Aff (err :: RequestError | eff) DeleteReportDefinitionResponse
```

Delete a specified report definition

#### `describeReportDefinitions`

``` purescript
describeReportDefinitions :: forall eff. DescribeReportDefinitionsRequest -> Aff (err :: RequestError | eff) DescribeReportDefinitionsResponse
```

Describe a list of report definitions owned by the account

#### `putReportDefinition`

``` purescript
putReportDefinition :: forall eff. PutReportDefinitionRequest -> Aff (err :: RequestError | eff) PutReportDefinitionResponse
```

Create a new report definition

#### `AWSRegion`

``` purescript
newtype AWSRegion
  = AWSRegion String
```

Region of customer S3 bucket.

#### `AdditionalArtifact`

``` purescript
newtype AdditionalArtifact
  = AdditionalArtifact String
```

Enable support for Redshift and/or QuickSight.

#### `AdditionalArtifactList`

``` purescript
newtype AdditionalArtifactList
  = AdditionalArtifactList (Array AdditionalArtifact)
```

A list of additional artifacts.

#### `CompressionFormat`

``` purescript
newtype CompressionFormat
  = CompressionFormat String
```

Preferred compression format for report.

#### `DeleteReportDefinitionRequest`

``` purescript
newtype DeleteReportDefinitionRequest
  = DeleteReportDefinitionRequest { "ReportName" :: NullOrUndefined (ReportName) }
```

Request of DeleteReportDefinition

#### `DeleteReportDefinitionResponse`

``` purescript
newtype DeleteReportDefinitionResponse
  = DeleteReportDefinitionResponse { "ResponseMessage" :: NullOrUndefined (DeleteResponseMessage) }
```

Response of DeleteReportDefinition

#### `DeleteResponseMessage`

``` purescript
newtype DeleteResponseMessage
  = DeleteResponseMessage String
```

A message indicates if the deletion is successful.

#### `DescribeReportDefinitionsRequest`

``` purescript
newtype DescribeReportDefinitionsRequest
  = DescribeReportDefinitionsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (GenericString) }
```

Request of DescribeReportDefinitions

#### `DescribeReportDefinitionsResponse`

``` purescript
newtype DescribeReportDefinitionsResponse
  = DescribeReportDefinitionsResponse { "ReportDefinitions" :: NullOrUndefined (ReportDefinitionList), "NextToken" :: NullOrUndefined (GenericString) }
```

Response of DescribeReportDefinitions

#### `DuplicateReportNameException`

``` purescript
newtype DuplicateReportNameException
  = DuplicateReportNameException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown when putting a report preference with a name that already exists.

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

A message to show the detail of the exception.

#### `GenericString`

``` purescript
newtype GenericString
  = GenericString String
```

A generic string.

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown on a known dependency failure.

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

The max number of results returned by the operation.

#### `PutReportDefinitionRequest`

``` purescript
newtype PutReportDefinitionRequest
  = PutReportDefinitionRequest { "ReportDefinition" :: ReportDefinition }
```

Request of PutReportDefinition

#### `PutReportDefinitionResponse`

``` purescript
newtype PutReportDefinitionResponse
  = PutReportDefinitionResponse {  }
```

Response of PutReportDefinition

#### `ReportDefinition`

``` purescript
newtype ReportDefinition
  = ReportDefinition { "ReportName" :: ReportName, "TimeUnit" :: TimeUnit, "Format" :: ReportFormat, "Compression" :: CompressionFormat, "AdditionalSchemaElements" :: SchemaElementList, "S3Bucket" :: S3Bucket, "S3Prefix" :: S3Prefix, "S3Region" :: AWSRegion, "AdditionalArtifacts" :: NullOrUndefined (AdditionalArtifactList) }
```

The definition of AWS Cost and Usage Report. Customer can specify the report name, time unit, report format, compression format, S3 bucket and additional artifacts and schema elements in the definition.

#### `ReportDefinitionList`

``` purescript
newtype ReportDefinitionList
  = ReportDefinitionList (Array ReportDefinition)
```

A list of report definitions.

#### `ReportFormat`

``` purescript
newtype ReportFormat
  = ReportFormat String
```

Preferred format for report.

#### `ReportLimitReachedException`

``` purescript
newtype ReportLimitReachedException
  = ReportLimitReachedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown when the number of report preference reaches max limit. The max number is 5.

#### `ReportName`

``` purescript
newtype ReportName
  = ReportName String
```

Preferred name for a report, it has to be unique. Must starts with a number/letter, case sensitive. Limited to 256 characters.

#### `S3Bucket`

``` purescript
newtype S3Bucket
  = S3Bucket String
```

Name of customer S3 bucket.

#### `S3Prefix`

``` purescript
newtype S3Prefix
  = S3Prefix String
```

Preferred report path prefix. Limited to 256 characters.

#### `SchemaElement`

``` purescript
newtype SchemaElement
  = SchemaElement String
```

Preference of including Resource IDs. You can include additional details about individual resource IDs in your report.

#### `SchemaElementList`

``` purescript
newtype SchemaElementList
  = SchemaElementList (Array SchemaElement)
```

A list of schema elements.

#### `TimeUnit`

``` purescript
newtype TimeUnit
  = TimeUnit String
```

The frequency on which report data are measured and displayed.

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown when providing an invalid input. eg. Put a report preference with an invalid report name, or Delete a report preference with an empty report name.


