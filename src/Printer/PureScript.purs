module Printer.PureScript where

import Prelude
import Data.Array (elemIndex)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), drop, dropWhile, joinWith, replace, replaceAll, take, toLower, toUpper)
import Data.StrMap (StrMap, toArrayWithKey)
import Node.Path (FilePath, concat)

import Aws (Service(..), ServiceMetadata(..), ServiceOperation(..), ServiceShape(..), ServiceShapeName(..))

clientFilePath :: FilePath -> ServiceMetadata -> Service -> FilePath
clientFilePath path (ServiceMetadata { name }) _ = concat [path, name <> ".purs"]

client :: ServiceMetadata -> Service -> String
client serviceMetadata (Service { operations, shapes }) =
    (header serviceMetadata) <>
    (toArrayWithKey (\name -> \serviceOperation -> function name serviceOperation) operations # joinWith "") <>
    (toArrayWithKey (\name -> \serviceShape -> record name serviceShape) shapes # joinWith "")

header :: ServiceMetadata -> String
header (ServiceMetadata serviceMetadata) = """
module Aws.{{serviceName}} where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import Aws.Service (AwsError, request)

serviceName = "{{serviceName}}" :: String
""" # replaceAll (Pattern "{{serviceName}}") (Replacement serviceMetadata.name)

function :: String -> ServiceOperation -> String
function name (ServiceOperation serviceOperation) = """
{{camelCaseName}} :: forall eff. {{input}} Aff (err :: AwsError | eff) {{output}}
{{camelCaseName}} = request serviceName "{{pascalCaseName}}" {{fallback}}
""" # replaceAll (Pattern "{{camelCaseName}}") (Replacement camelCaseName)
    # replace (Pattern "{{pascalCaseName}}") (Replacement pascalCaseName)
    # replace (Pattern "{{input}}") (Replacement input)
    # replace (Pattern "{{output}}") (Replacement output)
    # replace (Pattern "{{fallback}}") (Replacement fallback)
        where
            camelCaseName = (take 1 name # toLower) <> (drop 1 name)
            pascalCaseName =  name
            input = unNullOrUndefined serviceOperation.input # maybe "" (\(ServiceShapeName { shape }) -> shape <> " ->")
            output =  unNullOrUndefined serviceOperation.output # maybe "Unit" (\(ServiceShapeName { shape }) -> shape)
            fallback = unNullOrUndefined serviceOperation.input # maybe "unit" (\_ -> "")

purescriptTypes = ["String", "Int", "Number", "Boolean"] :: Array String

compatibleType :: String -> String
compatibleType type' = safeType
    where
        typeNoPrefix = dropWhile (_ == '_') type'
        typePascalCase = (take 1 typeNoPrefix # toUpper) <> (drop 1 typeNoPrefix)
        typeNotJs = case typePascalCase of
            "Blob" -> "String"
            "Integer" -> "Int"
            "Long" -> "Number"
            "Float" -> "Number"
            "Double" -> "Number"
            "Timestamp" -> "Number"

            "Function" -> "Function'"
            "Map" -> "Map'"
            "Record" -> "Record'"
            "Unit" -> "Unit'"

            validType -> validType

        safeType = if (elemIndex typeNotJs purescriptTypes # isJust) || (type' == typeNotJs)
            then typeNotJs
            else typeNotJs <> "'"

record :: String -> ServiceShape -> String
record name serviceShape = output
    where
        type' = compatibleType name
        output = if (elemIndex type' purescriptTypes # isJust)
            then ""
            else record' type' serviceShape

record' :: String -> ServiceShape -> String
record' name serviceShape = """
newtype {{name}} = {{name}} {{type}}
""" # replaceAll (Pattern "{{name}}") (Replacement $ name)
    # replace (Pattern "{{type}}") (Replacement $ recordType serviceShape)

recordType :: ServiceShape -> String
recordType (ServiceShape serviceShape) = case serviceShape of
    { "type": "list", member: NullOrUndefined (Just (shape)) } -> recordArray shape
    { "type": "map", key: NullOrUndefined (Just key), value: NullOrUndefined (Just value) } -> recordMap key value
    { "type": "structure", members: NullOrUndefined (Just members) } -> recordRecord members
    { "type": type' } -> compatibleType type'

recordArray :: ServiceShapeName -> String
recordArray (ServiceShapeName { shape }) = "(Array " <> (compatibleType shape) <> ")"

recordMap :: ServiceShapeName -> ServiceShapeName -> String
recordMap (ServiceShapeName key) (ServiceShapeName value) = "(Map " <> compatibleKey <> " " <> compatibleValue <> ")"
    where
        compatibleKey = compatibleType key.shape
        compatibleValue = compatibleType value.shape

recordRecord :: StrMap ServiceShapeName -> String
recordRecord keyValue = " { " <> properties <> " } "
    where
        property = (\key -> \(ServiceShapeName { shape }) -> "\"" <> (compatibleType key) <> "\" :: NullOrUndefined " <> (compatibleType shape))
        properties = toArrayWithKey property keyValue # joinWith ", "
