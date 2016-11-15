{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Type of document structure for pretty printing the lomakes
module Lomake.Pretty where

-- TODO: use builder

import Prelude ()
import Futurice.Prelude
import Control.Lens     (maximumOf, filtered, to)

import qualified Data.Text as T

data Section = Section
    { _secName   :: !Text
    , _secFields :: [(Text, Field)]
    }

data Field
    = ShortField Text -- ^ One liner
    | LongField Text  -- ^ Multi line
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
    isDisplayableField (_, LongField f) = not (T.null f)
    isDisplayableField _                 = False

    renderSection (Section n fs) =
        n <> "\n" <> T.map (const '=') n <> "\n" <>
        foldMap (uncurry renderField) fs <> "\n"

    renderField n (ShortField f) | not (T.null f) =
        n <> ": " <> T.replicate (maxWidth - T.length n + 2) "." <> " " <>  f <> "\n"
    renderField n (LongField f) | not (T.null f) =
        n <> ":\n" <>
        f <> "\n\n"

    renderField _ _ = ""

