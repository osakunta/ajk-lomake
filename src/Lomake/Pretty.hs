{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Type of document structure for pretty printing the lomakes
module Lomake.Pretty where

-- TODO: use builder

import Control.Lens     (filtered, maximumOf, to)
import Futurice.Prelude
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.Text       as T

data Section = Section
    { _secName   :: !Text
    , _secFields :: [(Text, Field)]
    }

data Field
    = ShortField Text -- ^ One liner
    | LongField Text  -- ^ Multi line
    | FileField BS.ByteString
    | EmptyField
  deriving (Eq)

makeLenses ''Section
makePrisms ''Field

render :: [Section] -> Text
render ss = foldMap renderSection ss
  where
    maxWidth = fromMaybe 0 $
        maximumOf (folded . secFields . folded . filtered isDisplayableField . _1 . to T.length) ss

    isDisplayableField (_, ShortField f) = not (T.null f)
    isDisplayableField (_, LongField f)  = not (T.null f)
    isDisplayableField (_, FileField _)  = True
    isDisplayableField (_, EmptyField)   = False

    renderSection (Section n fs) =
        n <> "\n" <> T.map (const '=') n <> "\n" <>
        foldMap (uncurry renderField) fs <> "\n"

    renderField n (ShortField f)
        | not (T.null f) =
            n <> ": " <> T.replicate (maxWidth - T.length n + 2) "." <> " " <>  f <> "\n"
        | otherwise = ""
    renderField n (LongField f)
        | not (T.null f) =
            n <> ":\n" <>
            f <> "\n\n"
        | otherwise = ""

    renderField n (FileField _) =
            n <> ": " <> T.replicate (maxWidth - T.length n + 2) "." <> " liitteenä\n"
    renderField _ EmptyField = ""

attachements :: [Section] -> [(Text, BS.ByteString)]
attachements = foldMap sect where
    sect (Section _ fs)      = foldMap field fs
    field (n, FileField lbs) = [(n, lbs)]
    field (_, _)             = []
