# purescript-aws-sdk

[![Latest release](https://img.shields.io/bower/v/purescript-aws-sdk.svg)](https://github.com/plippe/purescript-aws-sdk/releases)

## Getting started

```sh
bower install purescript-aws-sdk
npm install aws-sdk # bower package seems broken :(
```

## Cycle in declaration

[AWS SDK JS](https://github.com/aws/aws-sdk-js) has a few types with properties referencing themselves. As a quick fix,
all properties that would raise `CycleInDeclaration` aren't in the generated code. Bellow are the missing properties:
  - `Or` in `AWS.CostExplorer.Expression`
  - `And` in `AWS.CostExplorer.Expression`
  - `Not` in `AWS.CostExplorer.Expression`
  - `L` in `AWS.DynamoDBStreams.AttributeValue`
  - `M` in `AWS.DynamoDBStreams.AttributeValue`
  - `Configurations` in `AWS.EMR.Configuration`
  - `Resources` in `AWS.Organizations.HandshakeResource`
  - `Aggregators` in `AWS.SSM.InventoryAggregator`

## Development

```sh
cat Makefile
```
