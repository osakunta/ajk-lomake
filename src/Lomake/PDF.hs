{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lomake.PDF where

import Prelude ()
import Futurice.Prelude           hiding (put, get)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class  (lift)
import Data.Maybe                 (listToMaybe)
import Graphics.PDF               (PDFFloat, PDFFont, PDFString)

import qualified Data.Text    as T
import qualified Graphics.PDF as PDF

import Lomake.Pretty

-------------------------------------------------------------------------------
-- Geometry
-------------------------------------------------------------------------------

-- https://www.gnu.org/software/gv/manual/html_node/Paper-Keywords-and-paper-size-in-points.html
a4rect :: PDF.PDFRect
a4rect = PDF.PDFRect 0 0 595 842

a4width :: PDFFloat
a4width = 595

a4height :: PDFFloat
a4height = 842

pointsPerInch :: PDFFloat
pointsPerInch = 72

lineHeight :: PDFFloat
lineHeight = 14

margin :: PDFFloat
margin = pointsPerInch

indent :: PDFFloat
indent = pointsPerInch / 4

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- | Each commands prints one line of text
data Command
    = Title PDFString
    | FieldTitle PDFString
    | OffsetRow PDFFloat PDFString
    | Row PDFString
    | SkipLine
    | UnSkipLine

calculateCommands :: [Section] -> [Command]
calculateCommands = concatMap calculateSection
  where
    calculateSection (Section n fs)
        = Title (textToPDFString n)
        : concatMap (uncurry calculateField) fs
        ++ [SkipLine]

    calculateField n (ShortField f) | not (T.null f) =
        [ FieldTitle n' ] ++
        [ UnSkipLine | PDF.textWidth fieldNameFont n' < w ] ++
        map (OffsetRow w) (textToPDFStrings w f)
      where
        n' = textToPDFString n
        w = a4width / 2 - margin

    calculateField n (LongField f) | not (T.null f)
        = FieldTitle (textToPDFString n)
        : concatMap (fmap Row . textToPDFStrings (a4width - indent - 2 * margin)) (T.lines f)
    calculateField _ _
        = []


-- | To prevent overflows, assumes single line text
textToPDFStrings
    :: PDFFloat -- ^ Available width
    -> Text
    -> [PDFString]
textToPDFStrings m = go . T.words
  where
    go :: [Text] -> [PDFString]
    go [] = []
    go (w : ws) =
        let (a, bs) = split w ws
        in a : go bs

    split w ws = fromMaybe (textToPDFString w, ws) $ listToMaybe
        [ (a', b)
        | (a, b) <- reverse $ halves $ w : ws
        , not (null a)
        , let a' = textToPDFString (T.unwords a)
        , PDF.textWidth fieldValueFont a' < m
        ]

textToPDFString :: Text -> PDFString
textToPDFString = PDF.toPDFString . T.unpack

halves :: [a] -> [([a], [a])]
halves [] = [([], [])]
halves ys@(x : xs) = ([], ys) :
    [ (x : a, b)
    | (a, b) <- halves xs
    ]

-------------------------------------------------------------------------------
-- Render
-------------------------------------------------------------------------------

renderPDFFooter :: Int -> Text -> PDF.Draw ()
renderPDFFooter n title
    = PDF.drawText
    $ PDF.text footerFont margin (margin / 2)
    $ textToPDFString
    $ title <> " - " <> "sivu " <> textShow (n + 1)

renderPDFText :: Text -> [Section] -> [PDF.Draw ()]
renderPDFText title sections = do
    let commands = Title (textToPDFString title) : calculateCommands sections
    let pageCommands = splitPages commands
    flip map pageCommands $ \cs -> flip evalStateT 0 $ traverse_ renderCommand cs
  where
    splitPages :: [Command] -> [[Command]]
    splitPages = go 0 []
      where
        go :: PDFFloat -> [Command] -> [Command] -> [[Command]]
        go _ []  [] = []
        go _ acc [] = [reverse acc]
        go y acc (UnSkipLine : cs) = go (y - lineHeight) (UnSkipLine : acc) cs
        go y acc cs | y >= a4height - 2 * margin = reverse acc : go 0 [] cs
        go y acc (c@(Title _) : cs)
            = go (y + 2 * lineHeight) (c : acc) cs
        go y acc (c : cs)
            = go (y + lineHeight) (c : acc) cs

    renderCommand :: Command -> StateT PDFFloat PDF.Draw ()

    renderCommand SkipLine = do
        y <- get
        put $ y + lineHeight

    renderCommand UnSkipLine = do
        y <- get
        put $ y - lineHeight

    renderCommand (Title s) = do
        y <- get
        put $ y + lineHeight + lineHeight
        let y' = a4height - margin - y
        let y'' = a4height - margin - y - (lineHeight / 2)
        lift $ PDF.drawText $
            PDF.text fieldNameFont margin y' s
        lift $ PDF.stroke  $
            PDF.Line margin y'' (a4width - margin) y''

    renderCommand (OffsetRow x v) = do
        y <- get
        put $ y + lineHeight
        let y' = a4height - margin - y

        lift $ PDF.drawText $
            PDF.text fieldValueFont (margin + x) y' v


    renderCommand (FieldTitle s) = do
        y <- get
        put $ y + lineHeight
        let y' = a4height - margin - y
        lift $ PDF.drawText $
            PDF.text fieldNameFont margin y' s

    renderCommand (Row s) = do
        y <- get
        put $ y + lineHeight
        let y' = a4height - margin - y
        lift $ PDF.drawText $
            PDF.text fieldValueFont (margin + indent) y' s

fieldNameFont :: PDFFont
fieldNameFont = PDF.PDFFont PDF.Helvetica_Bold 10

fieldValueFont :: PDFFont
fieldValueFont = PDF.PDFFont PDF.Helvetica 10

footerFont :: PDFFont
footerFont = PDF.PDFFont PDF.Helvetica 8
