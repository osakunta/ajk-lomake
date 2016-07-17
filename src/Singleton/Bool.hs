{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Singleton.Bool where

data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

class    SBoolI (b :: Bool) where sbool :: SBool b
instance SBoolI 'True       where sbool = STrue
instance SBoolI 'False      where sbool = SFalse
