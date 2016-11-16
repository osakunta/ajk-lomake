{-# LANGUAGE OverloadedStrings #-}
module Lomake.PDF where

import Prelude ()
import Futurice.Prelude
-- import Control.Lens     (maximumOf, filtered, to)

import qualified Data.Text as T
import qualified Graphics.PDF             as PDF

import Lomake.Pretty

-- https://www.gnu.org/software/gv/manual/html_node/Paper-Keywords-and-paper-size-in-points.html
a4rect :: PDF.PDFRect
a4rect = PDF.PDFRect 0 0 595 842

a4width :: PDF.PDFFloat
a4width = 595

a4height :: PDF.PDFFloat
a4height = 842

pointsPerInch :: PDF.PDFFloat
pointsPerInch = 72

renderPDFText :: Text -> [Section] -> PDF.PDFText ()
renderPDFText title sections = do
    PDF.setFont fieldNameFont
    PDF.displayText $ PDF.toPDFString $ T.unpack title
    PDF.startNewLine
    traverse_ renderSection sections
  where
    renderSection (Section n fs) = do
        PDF.setFont fieldNameFont
        PDF.displayText $ PDF.toPDFString $ T.unpack n
        PDF.startNewLine

        traverse_ (uncurry renderField) fs
        PDF.startNewLine

    renderField n (ShortField f) | not (T.null f) = do
        PDF.setFont fieldNameFont
        PDF.displayText $ PDF.toPDFString $ T.unpack $ n <> ": "
        PDF.setFont fieldValueFont
        PDF.displayText $ PDF.toPDFString $ T.unpack f
        PDF.startNewLine

    renderField n (LongField f) | not (T.null f) = do
        PDF.setFont fieldNameFont
        PDF.displayText $ PDF.toPDFString $ T.unpack $ n <> ":"
        PDF.startNewLine
        PDF.setFont fieldValueFont
        for_ (T.lines f) $ \l -> do
            PDF.displayText $ PDF.toPDFString $ T.unpack l
            PDF.startNewLine
        PDF.startNewLine

    renderField _ _ = pure ()

    fieldNameFont = PDF.PDFFont PDF.Helvetica_Bold 10
    fieldValueFont = PDF.PDFFont PDF.Helvetica 10
