

-- | <p>Provides translation between English and one of six languages, or between one of the six languages and English.</p>
module AWS.Translate where

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

serviceName = "Translate" :: String


-- | <p>Translates input text from the source language to the target language. You can translate between English (en) and one of the following languages, or between one of the following languages and English.</p> <ul> <li> <p>Arabic (ar)</p> </li> <li> <p>Chinese (Simplified) (zh)</p> </li> <li> <p>French (fr)</p> </li> <li> <p>German (de)</p> </li> <li> <p>Portuguese (pt)</p> </li> <li> <p>Spanish (es)</p> </li> </ul>
translateText :: forall eff. TranslateTextRequest -> Aff (exception :: EXCEPTION | eff) TranslateTextResponse
translateText = Request.request serviceName "translateText" 


newtype BoundedLengthString = BoundedLengthString String
derive instance newtypeBoundedLengthString :: Newtype BoundedLengthString _
derive instance repGenericBoundedLengthString :: Generic BoundedLengthString _
instance showBoundedLengthString :: Show BoundedLengthString where
  show = genericShow
instance decodeBoundedLengthString :: Decode BoundedLengthString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoundedLengthString :: Encode BoundedLengthString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal server error occurred. Retry your request.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerException :: Newtype InternalServerException _
derive instance repGenericInternalServerException :: Generic InternalServerException _
instance showInternalServerException :: Show InternalServerException where
  show = genericShow
instance decodeInternalServerException :: Decode InternalServerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerException :: Encode InternalServerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request is invalid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LanguageCodeString = LanguageCodeString String
derive instance newtypeLanguageCodeString :: Newtype LanguageCodeString _
derive instance repGenericLanguageCodeString :: Generic LanguageCodeString _
instance showLanguageCodeString :: Show LanguageCodeString where
  show = genericShow
instance decodeLanguageCodeString :: Decode LanguageCodeString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguageCodeString :: Encode LanguageCodeString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Translate is unavailable. Retry your request later.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The size of the input text exceeds the length constraint for the <code>Text</code> field. Try again with a shorter text. </p>
newtype TextSizeLimitExceededException = TextSizeLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTextSizeLimitExceededException :: Newtype TextSizeLimitExceededException _
derive instance repGenericTextSizeLimitExceededException :: Generic TextSizeLimitExceededException _
instance showTextSizeLimitExceededException :: Show TextSizeLimitExceededException where
  show = genericShow
instance decodeTextSizeLimitExceededException :: Decode TextSizeLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextSizeLimitExceededException :: Encode TextSizeLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of requests exceeds the limit. Resubmit your request later.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TranslateTextRequest = TranslateTextRequest 
  { "Text" :: (BoundedLengthString)
  , "SourceLanguageCode" :: (LanguageCodeString)
  , "TargetLanguageCode" :: (LanguageCodeString)
  }
derive instance newtypeTranslateTextRequest :: Newtype TranslateTextRequest _
derive instance repGenericTranslateTextRequest :: Generic TranslateTextRequest _
instance showTranslateTextRequest :: Show TranslateTextRequest where
  show = genericShow
instance decodeTranslateTextRequest :: Decode TranslateTextRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTranslateTextRequest :: Encode TranslateTextRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TranslateTextResponse = TranslateTextResponse 
  { "TranslatedText" :: (String)
  , "SourceLanguageCode" :: (LanguageCodeString)
  , "TargetLanguageCode" :: (LanguageCodeString)
  }
derive instance newtypeTranslateTextResponse :: Newtype TranslateTextResponse _
derive instance repGenericTranslateTextResponse :: Generic TranslateTextResponse _
instance showTranslateTextResponse :: Show TranslateTextResponse where
  show = genericShow
instance decodeTranslateTextResponse :: Decode TranslateTextResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTranslateTextResponse :: Encode TranslateTextResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Translate cannot translate input text in the source language into this target language. For more information, see <a>how-to-error-msg</a>. </p>
newtype UnsupportedLanguagePairException = UnsupportedLanguagePairException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedLanguagePairException :: Newtype UnsupportedLanguagePairException _
derive instance repGenericUnsupportedLanguagePairException :: Generic UnsupportedLanguagePairException _
instance showUnsupportedLanguagePairException :: Show UnsupportedLanguagePairException where
  show = genericShow
instance decodeUnsupportedLanguagePairException :: Decode UnsupportedLanguagePairException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedLanguagePairException :: Encode UnsupportedLanguagePairException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
