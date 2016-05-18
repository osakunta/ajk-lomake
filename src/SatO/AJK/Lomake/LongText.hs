{-# LANGUAGE OverloadedStrings   #-}
module SatO.AJK.Lomake.LongText where

import Data.Semigroup ((<>))
import Data.Text      (Text)
import Lomake
import Lucid

newtype LongText = LongText Text
instance LomakeField LongText where
    lomakeFieldView _ env name =
       textarea_ [name_ name] $ toHtml $ submittedTextValue env name
    lomakeFieldPretty (LongText t) = "\n" <> lomakeFieldPretty t <> "\n"
    lomakeFieldValidate _proxyA proxyReq = ofmap proxyReq LongText . lomakeText proxyReq
