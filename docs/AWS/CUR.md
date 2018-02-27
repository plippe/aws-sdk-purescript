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

##### Instances
``` purescript
Newtype AWSRegion _
```

#### `AdditionalArtifact`

``` purescript
newtype AdditionalArtifact
  = AdditionalArtifact String
```

Enable support for Redshift and/or QuickSight.

##### Instances
``` purescript
Newtype AdditionalArtifact _
```

#### `AdditionalArtifactList`

``` purescript
newtype AdditionalArtifactList
  = AdditionalArtifactList (Array AdditionalArtifact)
```

A list of additional artifacts.

##### Instances
``` purescript
Newtype AdditionalArtifactList _
```

#### `CompressionFormat`

``` purescript
newtype CompressionFormat
  = CompressionFormat String
```

Preferred compression format for report.

##### Instances
``` purescript
Newtype CompressionFormat _
```

#### `DeleteReportDefinitionRequest`

``` purescript
newtype DeleteReportDefinitionRequest
  = DeleteReportDefinitionRequest { "ReportName" :: NullOrUndefined (ReportName) }
```

Request of DeleteReportDefinition

##### Instances
``` purescript
Newtype DeleteReportDefinitionRequest _
```

#### `DeleteReportDefinitionResponse`

``` purescript
newtype DeleteReportDefinitionResponse
  = DeleteReportDefinitionResponse { "ResponseMessage" :: NullOrUndefined (DeleteResponseMessage) }
```

Response of DeleteReportDefinition

##### Instances
``` purescript
Newtype DeleteReportDefinitionResponse _
```

#### `DeleteResponseMessage`

``` purescript
newtype DeleteResponseMessage
  = DeleteResponseMessage String
```

A message indicates if the deletion is successful.

##### Instances
``` purescript
Newtype DeleteResponseMessage _
```

#### `DescribeReportDefinitionsRequest`

``` purescript
newtype DescribeReportDefinitionsRequest
  = DescribeReportDefinitionsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (GenericString) }
```

Request of DescribeReportDefinitions

##### Instances
``` purescript
Newtype DescribeReportDefinitionsRequest _
```

#### `DescribeReportDefinitionsResponse`

``` purescript
newtype DescribeReportDefinitionsResponse
  = DescribeReportDefinitionsResponse { "ReportDefinitions" :: NullOrUndefined (ReportDefinitionList), "NextToken" :: NullOrUndefined (GenericString) }
```

Response of DescribeReportDefinitions

##### Instances
``` purescript
Newtype DescribeReportDefinitionsResponse _
```

#### `DuplicateReportNameException`

``` purescript
newtype DuplicateReportNameException
  = DuplicateReportNameException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown when putting a report preference with a name that already exists.

##### Instances
``` purescript
Newtype DuplicateReportNameException _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

A message to show the detail of the exception.

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `GenericString`

``` purescript
newtype GenericString
  = GenericString String
```

A generic string.

##### Instances
``` purescript
Newtype GenericString _
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown on a known dependency failure.

##### Instances
``` purescript
Newtype InternalErrorException _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

The max number of results returned by the operation.

##### Instances
``` purescript
Newtype MaxResults _
```

#### `PutReportDefinitionRequest`

``` purescript
newtype PutReportDefinitionRequest
  = PutReportDefinitionRequest { "ReportDefinition" :: ReportDefinition }
```

Request of PutReportDefinition

##### Instances
``` purescript
Newtype PutReportDefinitionRequest _
```

#### `PutReportDefinitionResponse`

``` purescript
newtype PutReportDefinitionResponse
  = PutReportDefinitionResponse {  }
```

Response of PutReportDefinition

##### Instances
``` purescript
Newtype PutReportDefinitionResponse _
```

#### `ReportDefinition`

``` purescript
newtype ReportDefinition
  = ReportDefinition { "ReportName" :: ReportName, "TimeUnit" :: TimeUnit, "Format" :: ReportFormat, "Compression" :: CompressionFormat, "AdditionalSchemaElements" :: SchemaElementList, "S3Bucket" :: S3Bucket, "S3Prefix" :: S3Prefix, "S3Region" :: AWSRegion, "AdditionalArtifacts" :: NullOrUndefined (AdditionalArtifactList) }
```

The definition of AWS Cost and Usage Report. Customer can specify the report name, time unit, report format, compression format, S3 bucket and additional artifacts and schema elements in the definition.

##### Instances
``` purescript
Newtype ReportDefinition _
```

#### `ReportDefinitionList`

``` purescript
newtype ReportDefinitionList
  = ReportDefinitionList (Array ReportDefinition)
```

A list of report definitions.

##### Instances
``` purescript
Newtype ReportDefinitionList _
```

#### `ReportFormat`

``` purescript
newtype ReportFormat
  = ReportFormat String
```

Preferred format for report.

##### Instances
``` purescript
Newtype ReportFormat _
```

#### `ReportLimitReachedException`

``` purescript
newtype ReportLimitReachedException
  = ReportLimitReachedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown when the number of report preference reaches max limit. The max number is 5.

##### Instances
``` purescript
Newtype ReportLimitReachedException _
```

#### `ReportName`

``` purescript
newtype ReportName
  = ReportName String
```

Preferred name for a report, it has to be unique. Must starts with a number/letter, case sensitive. Limited to 256 characters.

##### Instances
``` purescript
Newtype ReportName _
```

#### `S3Bucket`

``` purescript
newtype S3Bucket
  = S3Bucket String
```

Name of customer S3 bucket.

##### Instances
``` purescript
Newtype S3Bucket _
```

#### `S3Prefix`

``` purescript
newtype S3Prefix
  = S3Prefix String
```

Preferred report path prefix. Limited to 256 characters.

##### Instances
``` purescript
Newtype S3Prefix _
```

#### `SchemaElement`

``` purescript
newtype SchemaElement
  = SchemaElement String
```

Preference of including Resource IDs. You can include additional details about individual resource IDs in your report.

##### Instances
``` purescript
Newtype SchemaElement _
```

#### `SchemaElementList`

``` purescript
newtype SchemaElementList
  = SchemaElementList (Array SchemaElement)
```

A list of schema elements.

##### Instances
``` purescript
Newtype SchemaElementList _
```

#### `TimeUnit`

``` purescript
newtype TimeUnit
  = TimeUnit String
```

The frequency on which report data are measured and displayed.

##### Instances
``` purescript
Newtype TimeUnit _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

This exception is thrown when providing an invalid input. eg. Put a report preference with an invalid report name, or Delete a report preference with an empty report name.

##### Instances
``` purescript
Newtype ValidationException _
```


