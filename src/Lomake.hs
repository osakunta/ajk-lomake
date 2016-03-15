{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell #-}
module Lomake (
    -- * Definition type
    D(..), unD, d, dToMaybe,
    -- * Form classes
    LomakeForm(..),
    LomakeSection(..),
    LomakeField(..),
    -- * Lomake environment
    LomakeEnv,
    emptyLomakeEnv,
    hiddenForm,
    submittedTextValue,
    -- * Lomake validation
    LomakeValidate,
    LomakeResult(..),
    runLomakeValidate,
    validate,
    lomakeText,
    requiredText,
    optionalText,
    -- * Enums
    -- | For enums it's trivial to define a 'LomakeField' instance:
    --
    -- @
    --instance LomakeField E where
    --    lomakeFieldView   = enumLomakeFieldView
    --    lomakeFieldValidate  = enumLomakeFieldValidate
    --    lomakeFieldPretty = text . humanShow
    -- @
    LomakeEnum(..),
    HumanShow(..),
    enumLomakeFieldView,
    enumLomakeFieldValidate,
    -- * Utilities
    -- ** Maybe Maybe
    O,
    otraverse,
    ofmap,
    -- ** Singleton bool
    SBool(..),
    SBoolI(..),
    -- * Pretty
    text,
    isEmpty,
    ($$),
    render,
    ) where

import Control.Monad  (forM_)
import Data.Map       (Map)
import Data.Maybe     (isJust, fromMaybe)
import Data.Semigroup ((<>))
import Data.String    (fromString)
import Data.Text      (Text)
import GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)
import Generics.SOP
import Lucid

import qualified Data.Map         as Map
import qualified Data.Text        as T
import qualified Servant

-- | Field description.
data D (sym :: Symbol) (req :: Bool) a = D (O req a)

d :: forall sym req a. SBoolI req => a -> D sym req a
d = case sbool (Proxy :: Proxy req) of
    STrue  -> D . id
    SFalse -> D . Just

unD :: D sym req a -> O req a
unD (D x) = x

-- | 'D' can be always flatten to 'Maybe'.
dToMaybe :: forall sym req a. SBoolI req => D sym req a -> Maybe a
dToMaybe = case sbool (Proxy :: Proxy req) of
    STrue  -> Just . unD
    SFalse -> unD

-------------------------------------------------------------------------------
-- Singleton boolean
-------------------------------------------------------------------------------

data SBool b where
    SFalse :: SBool 'False
    STrue  :: SBool 'True

class SBoolI b where sbool :: Proxy b -> SBool b
instance SBoolI 'True where sbool _ = STrue
instance SBoolI 'False where sbool _ = SFalse

lowerSBool :: SBoolI b => Proxy b -> Bool
lowerSBool p = case sbool p of
    SFalse -> False
    STrue  -> True

-------------------------------------------------------------------------------
-- O - I or Maybe
-------------------------------------------------------------------------------

type family O r a :: * where
    O 'True a  = a
    O 'False a = Maybe a

otraverse
    :: forall req f a b. (SBoolI req, Applicative f)
    => Proxy req
    -> (a -> f b)
    -> O req a -> f (O req b)
otraverse p = case sbool p of
    STrue  -> id
    SFalse -> traverse

ofmap
    :: forall req f a b. (SBoolI req, Functor f)
    => Proxy req
    -> (a -> b)
    -> f (O req a) -> f (O req b)
ofmap p = case sbool p of
    STrue  -> fmap
    SFalse -> (fmap . fmap)

-------------------------------------------------------------------------------
-- HumanShow
-------------------------------------------------------------------------------

class HumanShow a where
    humanShow :: a -> String

-------------------------------------------------------------------------------
-- SOP utilities
-------------------------------------------------------------------------------

fieldInfos :: DatatypeInfo '[xs] -> NP FieldInfo xs
fieldInfos (ADT _ _ (cs :* Nil)) = fieldInfos' cs
fieldInfos (Newtype _ _ cs)      = fieldInfos' cs
fieldInfos _                     = error "fieldInfos: impossible happened"

fieldInfos' :: ConstructorInfo xs -> NP FieldInfo xs
fieldInfos' (Constructor _name) = hpure (FieldInfo "")
fieldInfos' (Infix _ _ _) = FieldInfo "_1" :* FieldInfo "_2" :* Nil
fieldInfos' (Record _ fs) = fs

-- | The environment resulted after parsing the inputs. Contains
-- warnings and original inputs.
data LomakeEnv = LomakeEnv
    { _lomakeEnvErrors :: Map Text [Text]
    , _lomakeEnvInputs :: [(Text, Text)]
    }

-- | Empty lomake environment, useful for @GET@ requests.
emptyLomakeEnv :: LomakeEnv
emptyLomakeEnv = LomakeEnv mempty []

class LomakeField a where
    lomakeFieldView
        :: Monad m
        => Proxy a
        -> LomakeEnv
        -> Text  -- ^ field name
        -> HtmlT m ()

    lomakeFieldValidate
        :: SBoolI req
        => Proxy a
        -> Proxy req
        -> Text  -- ^ field name
        -> LomakeValidate (O req a)

    lomakeFieldPretty
        :: a
        -> Doc

hasErrors :: LomakeEnv -> Text -> Bool
hasErrors (LomakeEnv errs _) name = isJust $ Map.lookup name errs

-- | The value posted, defaults to @""@
submittedTextValue :: LomakeEnv -> Text -> Text
submittedTextValue (LomakeEnv _ params) name = fromMaybe "" $
    lookup name params

-- | This can be used to resubmit the form.
hiddenForm :: Monad m => LomakeEnv -> HtmlT m ()
hiddenForm (LomakeEnv _ inputs) = forM_ inputs $ \(k, v) ->
    input_ [type_ "hidden", name_ k, value_ v]

class LomakeField' a where
    lomakeFieldView'
        :: Monad m
        => Proxy a
        -> LomakeEnv
        -> String -- ^ Field name
        -> HtmlT m ()

    lomakeFieldValidate'
        :: String          -- ^ Field name
        -> LomakeValidate a

    lomakeFieldPretty'
        :: a
        -> Doc

instance (LomakeField a, KnownSymbol sym, SBoolI req) => LomakeField' (D sym req a) where
    lomakeFieldView' _ env name = div_ [class_ cls] $ do
        div_ [class_ "large-4 columns"] $ do
            label_ [class_ "text-right middle", for_ $ T.pack name] $ toHtml desc'
        div_ [class_ "large-8 columns"] $ lomakeFieldView proxyA env (T.pack name)
      where
        proxyA = Proxy :: Proxy a
        proxySym = Proxy :: Proxy sym
        proxyReq = Proxy :: Proxy req
        desc = T.pack (symbolVal proxySym)
        desc' = if (lowerSBool proxyReq)
                    then desc <> "*"
                    else desc
        cls = if hasErrors env (T.pack name) then "row error" else "row"

    lomakeFieldValidate' name = D <$> (lomakeFieldValidate (Proxy :: Proxy a) (Proxy :: Proxy req)  (T.pack name))

    lomakeFieldPretty' x = case sbool (Proxy :: Proxy req) of
        STrue -> case x of
            D val -> p val
        SFalse -> case x of
            D (Just val) -> p val
            D Nothing    -> ""
      where
        p :: a -> Doc
        p val =
            let doc = lomakeFieldPretty val
            in if isEmpty doc
                then ""
                else text (desc ++ ":") <+> pad <+> doc

        proxySym = Proxy :: Proxy sym
        desc = symbolVal proxySym
        pad = text $ replicate (40 - length desc) '.'

-------------------------------------------------------------------------------
-- SOP magic
-------------------------------------------------------------------------------

sopView
    :: forall a xs m. (Generic a, HasDatatypeInfo a, Code a ~ '[xs], All LomakeField' xs, Monad m)
    => Proxy a -> LomakeEnv -> HtmlT m ()
sopView _ env =
    sopView' (fieldInfos (datatypeInfo (Proxy :: Proxy a)))
  where
    sopView' :: forall ys. (All LomakeField' ys) => NP FieldInfo ys -> HtmlT m ()
    sopView' Nil         = pure ()
    sopView' fs@(_ :* _) = sopView'' fs
      where
        sopView'' :: forall z zs. (LomakeField' z, All LomakeField' zs) => NP FieldInfo (z ': zs) -> HtmlT m ()
        sopView'' (FieldInfo name :* t) = do
            lomakeFieldView' (Proxy :: Proxy z) env name
            sopView' t

sopForm
    :: forall a xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs],All LomakeField' xs)
    => LomakeValidate a
sopForm = to . SOP . Z <$> hsequence (sopForm' (fieldInfos (datatypeInfo (Proxy :: Proxy a))))
  where
    sopForm' :: forall ys. All LomakeField' ys => NP FieldInfo ys -> NP LomakeValidate ys
    sopForm' = hcmap (Proxy :: Proxy LomakeField') f

    f :: forall b. LomakeField' b => FieldInfo b -> LomakeValidate b
    f (FieldInfo name) = lomakeFieldValidate' name

sopPretty
    :: forall a xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs],  All LomakeField' xs)
    => a -> Doc
sopPretty x = case from x of
    (SOP (Z x')) -> sopPretty' x'
    _            -> error "sopPretty: impossible happened"
  where
    sopPretty' :: forall ys. All LomakeField' ys => NP I ys -> Doc
    sopPretty' Nil = mempty
    sopPretty' (I y :* ys) = lomakeFieldPretty' y $$ sopPretty' ys

-------------------------------------------------------------------------------
-- LomakeValidate
-------------------------------------------------------------------------------

-- | Lomake forms are 'Applicative'.
newtype LomakeValidate a = LomakeValidate ([(Text, Text)] -> (Map Text [Text], Maybe a))

data LomakeResult a = LomakeResult LomakeEnv (Maybe a)

runLomakeValidate :: LomakeValidate a -> [(Text, Text)] -> LomakeResult a
runLomakeValidate (LomakeValidate f) params =
    case f params of
        (errs, x) -> LomakeResult (LomakeEnv errs params) x

instance Functor LomakeValidate where
    fmap f (LomakeValidate l)  = LomakeValidate $ (fmap . fmap . fmap) f l

instance Applicative LomakeValidate where
    pure x = LomakeValidate $ \_ -> (Map.empty, Just x)
    LomakeValidate f <*> LomakeValidate x = LomakeValidate $ \p ->
        case (f p, x p) of
            ((errF, valF), (errX, valX)) ->
                (Map.unionWith mappend errF errX, valF <*> valX)

requiredText
    :: Text
    -> LomakeValidate Text
requiredText name = LomakeValidate $ \p ->
    case lookup name p of
        Nothing ->
            (Map.singleton name ["Pakollinen kenttä"], Nothing)
        Just x | T.null x  -> do
            (Map.singleton name ["Pakollinen kenttä"], Nothing)
               | otherwise -> (Map.empty, Just x)

-- | Optional value, doesn't issue any warnings.
optionalText
    :: Text
    -> LomakeValidate (Maybe Text)
optionalText name = LomakeValidate $ \p ->
    (Map.empty, Just $ lookup name p)

-- | Analogue to 'mapMaybe', 'Left' value is a warning issued.
validate :: (a -> Either (Text, Text) b) -> LomakeValidate a -> LomakeValidate b
validate f (LomakeValidate l) = LomakeValidate $ \p ->
    case l p of
        (errs, Nothing) -> (errs, Nothing)
        (errs, Just x)  -> case f x of
            Right y     -> (errs, Just y)
            Left (n, m) -> (Map.unionWith mappend errs (Map.singleton n [m]), Nothing)

-- | Generalisation of 'requiredText' and 'optionalText'.
lomakeText
    :: forall req. (SBoolI req)
    => Proxy req
    -> Text
    -> LomakeValidate (O req Text)
lomakeText p = case sbool p of
    STrue  -> requiredText
    SFalse -> optionalText


-------------------------------------------------------------------------------
-- Section
-------------------------------------------------------------------------------

class LomakeSection a where
    lomakeSectionView :: Monad m => Proxy a -> LomakeEnv -> HtmlT m ()
    default lomakeSectionView
        :: forall xs m. (Generic a, HasDatatypeInfo a, Code a ~ '[xs], All LomakeField' xs, Monad m)
        => Proxy a -> LomakeEnv -> HtmlT m ()
    lomakeSectionView = sopView

    lomakeSectionForm :: LomakeValidate a
    default lomakeSectionForm
        :: forall xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs],All LomakeField' xs)
        => LomakeValidate a
    lomakeSectionForm = sopForm

    lomakeSectionPretty :: a -> Doc
    default lomakeSectionPretty
        :: forall xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs],  All LomakeField' xs)
        => a -> Doc
    lomakeSectionPretty = sopPretty

class LomakeSection' a where
    lomakeSectionView' :: Monad m => Proxy a -> LomakeEnv -> HtmlT m ()
    lomakeSectionForm' :: LomakeValidate a
    lomakeSectionPretty' :: a -> Doc

instance (LomakeSection a, KnownSymbol sym) => LomakeSection' (D sym 'True a) where
    lomakeSectionView' _ env = div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
        h2_ $ fromString $ symbolVal (Proxy :: Proxy sym)
        lomakeSectionView (Proxy :: Proxy a) env

    lomakeSectionForm' = D <$> lomakeSectionForm

    lomakeSectionPretty' (D x) =
        text name $$ text ('=' <$ name) $$ lomakeSectionPretty x $$ text "\n"
      where
        name = symbolVal (Proxy :: Proxy sym)

sopFormView
    :: forall a xs m. (Generic a, Code a ~ '[xs], All LomakeSection' xs, Monad m)
    => Proxy a -> LomakeEnv -> HtmlT m ()
sopFormView _ env = sopView' (sList :: SList xs)
  where
    sopView' :: forall ys. All LomakeSection' ys => SList ys -> HtmlT m ()
    sopView' SNil  = pure ()
    sopView' SCons = sopView''
      where
        sopView'' :: forall z zs. ((z ': zs) ~ ys) => HtmlT m ()
        sopView'' = do
            lomakeSectionView' (Proxy :: Proxy z) env
            sopView' (sList :: SList zs)

sopFormValidate
    :: forall a xs. (Generic a, Code a ~ '[xs], All LomakeSection' xs)
    => LomakeValidate a
sopFormValidate = to . SOP . Z <$> hsequence sopValidate'
  where
    sopValidate' :: forall ys. All LomakeSection' ys => NP LomakeValidate ys
    sopValidate' = hcpure (Proxy :: Proxy LomakeSection') lomakeSectionForm'

sopFormPretty
    :: forall a xs. (Generic a, Code a ~ '[xs],  All LomakeSection' xs)
    => a -> Doc
sopFormPretty x = case from x of
    SOP (Z x') -> sopPretty' x'
    _          -> error "sopFormPretty: impossible happened"
  where
    sopPretty' :: forall ys. All LomakeSection' ys => NP I ys -> Doc
    sopPretty' Nil = mempty
    sopPretty' (I y :* ys) = lomakeSectionPretty' y $$ sopPretty' ys

-------------------------------------------------------------------------------
-- Form
-------------------------------------------------------------------------------

class LomakeForm a where
    lomakeView   :: Monad m => Proxy a -> LomakeEnv -> HtmlT m ()
    default lomakeView
        :: forall xs m. (Generic a, Code a ~ '[xs], All LomakeSection' xs, Monad m)
        => Proxy a -> LomakeEnv -> HtmlT m ()
    lomakeView = sopFormView

    lomakeValidate  :: LomakeValidate a
    default lomakeValidate
        :: forall xs. (Generic a, Code a ~ '[xs], All LomakeSection' xs)
        => LomakeValidate a
    lomakeValidate = sopFormValidate

    lomakePretty    :: a -> Doc
    default lomakePretty
        :: forall xs. (Generic a, Code a ~ '[xs],  All LomakeSection' xs)
        => a -> Doc
    lomakePretty = sopFormPretty

instance LomakeForm a => Servant.FromFormUrlEncoded (LomakeResult a) where
    fromFormUrlEncoded = Right . runLomakeValidate lomakeValidate

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

instance LomakeField Text where
    lomakeFieldPretty = text . T.unpack
    lomakeFieldView _ env name =
        input_ [type_ "text", name_  name, value_ $ submittedTextValue env name]
    lomakeFieldValidate _ = lomakeText

-------------------------------------------------------------------------------
-- Bool
-------------------------------------------------------------------------------

instance HumanShow Bool where
    humanShow = bool "Kyllä" "Ei"

instance LomakeEnum Bool where
    universe = [True, False]

instance LomakeField Bool where
    lomakeFieldView = enumLomakeFieldView
    lomakeFieldValidate = enumLomakeFieldValidate
    lomakeFieldPretty = text . humanShow

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

-------------------------------------------------------------------------------
-- Enum
-------------------------------------------------------------------------------

enumLomakeFieldValidate
    :: forall a req. (Enum a, Bounded a, Show a, SBoolI req)
    => Proxy a
    -> Proxy req
    -> Text
    -> LomakeValidate (O req a)
enumLomakeFieldValidate _proxyA proxyReq name = validate (otraverse proxyReq f) (lomakeText proxyReq name)
  where
    f k = maybe (err k) pure $ lookup k pairs
    err k = Left (name, "Invalid value: " <> k)
    values = [minBound..maxBound] :: [a]
    pairs = fmap (\v -> (T.pack $ show v, v)) values

-- | Finite enumeration. Order of 'universe' is used in generated view.
class LomakeEnum a where
    universe :: [a]
    default universe :: (Enum a, Bounded a) => [a]
    universe = [minBound..maxBound]

enumLomakeFieldView
    :: forall a m. (Monad m, LomakeEnum a, Show a, HumanShow a)
    => Proxy a
    -> LomakeEnv
    -> Text  -- ^ field name
    -> HtmlT m ()
enumLomakeFieldView _ env name = forMSep_ values "" $ \v ->
      label_ $ do
          let c = if submittedTextValue env name == (T.pack $ show v)
                  then [checked_]
                  else []
          input_ $ [type_ "radio", name_  name, value_ $ T.pack $ show v] ++ c
          span_ $ toHtml $ humanShow v
  where
    values = universe :: [a]

forMSep_ :: Applicative m => [a] -> m c -> (a -> m b) -> m ()
forMSep_ [] _ _     = pure ()
forMSep_ [x] _ f    = f x *> pure ()
forMSep_ (x:xs) s f = f x *> s *> forMSep_ xs s f

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

type Doc = Text

text :: String -> Doc
text = T.pack

($$) :: Doc -> Doc -> Doc
a $$ b
    | T.null a  = b
    | T.null b  = a
    | otherwise = a <> "\n" <> b

(<+>) :: Doc -> Doc -> Doc
a <+> b = a <> " " <> b

isEmpty :: Doc -> Bool
isEmpty = T.null

render :: Doc -> String
render = T.unpack
