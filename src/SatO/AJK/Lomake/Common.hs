{-# LANGUAGE OverloadedStrings    #-}
module SatO.AJK.Lomake.Common where

import Lomake

import qualified Data.Text as T

data Siv = Naimaton | Aviossa
    deriving (Eq, Show, Enum, Bounded)

data Jasen = Varsinainen | Ulko
    deriving (Eq, Show, Enum, Bounded)

data Parents = BothAlive | OneAlive | Dead | YH
    deriving (Eq, Show, Enum, Bounded)

data Toistaiseksi = Toistaiseksi | Maaraikaiseksi
    deriving (Eq, Show, Enum, Bounded)

-------------------------------------------------------------------------------
-- Siv
-------------------------------------------------------------------------------

instance HumanShow Siv where
    humanShow = T.pack . show

instance LomakeEnum Siv

instance LomakeField Siv where
    lomakeFieldView   = enumLomakeFieldView
    lomakeFieldValidate  = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Parents
-------------------------------------------------------------------------------

instance HumanShow Parents where
    humanShow BothAlive = "Molemmat elävät"
    humanShow OneAlive  = "Vain toinen elää"
    humanShow Dead      = "Molemmat kuolleet"
    humanShow YH        = "Yh. äiti/isä"

instance LomakeEnum Parents

instance LomakeField Parents where
    lomakeFieldView   = enumLomakeFieldView
    lomakeFieldValidate  = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Jasen
-------------------------------------------------------------------------------

instance HumanShow Jasen where
    humanShow Varsinainen  = "Varsinainen jäsen"
    humanShow Ulko         = "Ulkojäsen"

instance LomakeEnum Jasen

instance LomakeField Jasen where
    lomakeFieldView = enumLomakeFieldView
    lomakeFieldValidate = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Toistaieksi
-------------------------------------------------------------------------------

instance HumanShow Toistaiseksi where
    humanShow Toistaiseksi   = "Toistaiseksi"
    humanShow Maaraikaiseksi = "Määräajaksi"

instance LomakeEnum Toistaiseksi

instance LomakeField Toistaiseksi where
    lomakeFieldView = enumLomakeFieldView
    lomakeFieldValidate = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow
