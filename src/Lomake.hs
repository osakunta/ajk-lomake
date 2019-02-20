{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- |
--
-- Note: Lomake is able to handle file uploads, but because we have
-- potentially two-step process, we embed the data as base16 encoded in the 
-- middle output. This is ugly, but this way we don't need to have
-- any state on the backend.
--
module Lomake (
    -- * Definition type
    D, D'(..), unD, d, dToMaybe,
    -- * Files
    F (..),
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
    ovalidate,
    lomakeText,
    requiredText,
    optionalText,
    -- * Enums
    -- | For enums it's trivial to define a 'LomakeField' instance:
    --
    -- @
    --instance LomakeField E where
    --    lomakeFieldView      = enumLomakeFieldView
    --    lomakeFieldValidate  = enumLomakeFieldValidate
    --    lomakeFieldPretty    = text . humanShow
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
    opure,
    -- ** Singleton bool
    Required(..),
    SRequired(..),
    SRequiredI(..),
    -- * Pretty
    module Lomake.Pretty,
    ) where

import Data.Either.Validation (Validation (..))
import Data.Foldable          (asum)
import Data.Map               (Map)
import Data.Maybe             (fromMaybe)
import Data.Semigroup         ((<>))
import Data.String            (fromString)
import Data.Text              (Text)
import Futurice.Prelude       hiding (Generic, from)
import Generics.SOP
import GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)
import Lucid                  hiding (for_)
import Prelude ()

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as Base16
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Foldable               as F
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as Map
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Lucid
import qualified Servant.Multipart           as MP
import qualified Web.FormUrlEncoded
import qualified Web.Internal.FormUrlEncoded as Form

import Lomake.Pretty

-------------------------------------------------------------------------------
-- D
-------------------------------------------------------------------------------

-- | Field description.
type D sym req a = D' sym req a ""

-- | Field description with extra info.
newtype D' (sym :: Symbol) (req :: Required) a (extra :: Symbol) = D (O req a)

d :: forall sym req a. SRequiredI req => a -> D sym req a
d = case srequired (Proxy :: Proxy req) of
    SRequired -> D . id
    SOptional -> D . Just

unD :: D' sym req a extra -> O req a
unD (D x) = x

-- | 'D' can be always flatten to 'Maybe'.
dToMaybe :: forall sym req a. SRequiredI req => D sym req a -> Maybe a
dToMaybe = case srequired (Proxy :: Proxy req) of
    SRequired  -> Just . unD
    SOptional -> unD

-------------------------------------------------------------------------------
-- F
-------------------------------------------------------------------------------

newtype F (sym :: Symbol) = F BS.ByteString

-------------------------------------------------------------------------------
-- Required
-------------------------------------------------------------------------------

data Required = Required | Optional

-------------------------------------------------------------------------------
-- Singleton boolean
-------------------------------------------------------------------------------

data SRequired (r :: Required) where
    SRequired :: SRequired 'Required
    SOptional :: SRequired 'Optional

class SRequiredI b where srequired :: Proxy b -> SRequired b
instance SRequiredI 'Required where srequired _ = SRequired
instance SRequiredI 'Optional where srequired _ = SOptional

lowerSRequired :: SRequiredI b => Proxy b -> Bool
lowerSRequired p = case srequired p of
    SOptional -> False
    SRequired  -> True

-------------------------------------------------------------------------------
-- O - I or Maybe
-------------------------------------------------------------------------------

type family O r a :: * where
    O 'Required a  = a
    O 'Optional a = Maybe a

otraverse
    :: forall req f a b. (SRequiredI req, Applicative f)
    => Proxy req
    -> (a -> f b)
    -> O req a -> f (O req b)
otraverse p = case srequired p of
    SRequired -> id
    SOptional -> traverse

ofmap
    :: forall req a b. (SRequiredI req)
    => Proxy req
    -> (a -> b)
    -> O req a -> O req b
ofmap p f = case srequired p of
    SRequired -> f
    SOptional -> fmap f

opure
    :: forall req a. (SRequiredI req)
    => Proxy req
    -> a
    -> O req a
opure p x = case srequired p of
    SRequired -> x
    SOptional -> Just x

-------------------------------------------------------------------------------
-- HumanShow
-------------------------------------------------------------------------------

class HumanShow a where
    humanShow :: a -> Text

-------------------------------------------------------------------------------
-- SOP utilities
-------------------------------------------------------------------------------

fieldInfos :: DatatypeInfo '[xs] -> NP FieldInfo xs
fieldInfos (ADT _ _ (cs :* Nil)) = fieldInfos' cs
fieldInfos (Newtype _ _ cs)      = fieldInfos' cs

fieldInfos' :: ConstructorInfo xs -> NP FieldInfo xs
fieldInfos' (Constructor _name) = hpure (FieldInfo "")
fieldInfos' (Infix _ _ _) = FieldInfo "_1" :* FieldInfo "_2" :* Nil
fieldInfos' (Record _ fs) = fs

-- | The environment resulted after parsing the inputs. Contains
-- warnings and original inputs.
data LomakeEnv = LomakeEnv
    { _lomakeEnvErrors :: Map Text [Text]
    , _lomakeEnvInputs :: [(Text, Text)]
    , _lomakeEnvFiles  :: [MP.FileData MP.Mem]
    }

-- | Empty lomake environment, useful for @GET@ requests.
emptyLomakeEnv :: LomakeEnv
emptyLomakeEnv = LomakeEnv mempty [] []

class LomakeField a where
    lomakeFieldView
        :: Monad m
        => Proxy a
        -> LomakeEnv
        -> Text  -- ^ field name
        -> HtmlT m ()

    lomakeFieldValidate
        :: SRequiredI req
        => Proxy a
        -> Proxy req
        -> Text  -- ^ field name
        -> LomakeValidate (O req a)

    lomakeFieldPretty
        :: a
        -> Field

hasErrors :: LomakeEnv -> Text -> Maybe [Text]
hasErrors (LomakeEnv errs _ _) name = Map.lookup name errs

-- | The value posted, defaults to @""@
submittedTextValue :: LomakeEnv -> Text -> Text
submittedTextValue (LomakeEnv _ params _) name = fromMaybe "" $
    lookup name params

-- | This can be used to resubmit the form.
hiddenForm :: Monad m => LomakeEnv -> HtmlT m ()
hiddenForm (LomakeEnv _ inputs files) = do
    for_ inputs $ \(k, v) ->
        input_ [type_ "hidden", name_ k, value_ v]
    for_ files $ \(MP.FileData k _ _ lbs) ->
        input_ [type_ "hidden", name_ k, value_ $ TE.decodeUtf8 $ Base16.encode $ LBS.toStrict lbs ]

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
        -> (Text, Field) -- field name and actual field

instance (LomakeField a, KnownSymbol sym, KnownSymbol extra, SRequiredI req)
    => LomakeField' (D' sym req a extra)
  where
    lomakeFieldView' _ env name = div_ [class_ cls] $ do
        div_ [class_ "large-4 columns"] $ do
            label_ [class_ "text-right middle-text-left middle", Lucid.for_ $ T.pack name] $ do
                toFancyHtml desc'
                when (not $ T.null extra) $ do
                    br_ []
                    small_ $ toHtml extra
                F.for_ errs $ \e -> do
                    br_ []
                    span_ $ toHtml e
        div_ [class_ "large-8 columns"] $ lomakeFieldView proxyA env (T.pack name)
      where
        proxyA = Proxy :: Proxy a
        proxySym = Proxy :: Proxy sym
        proxyReq = Proxy :: Proxy req
        proxyExtra = Proxy :: Proxy extra
        extra = T.pack (symbolVal proxyExtra)
        desc = T.pack (symbolVal proxySym)
        desc' = if (lowerSRequired proxyReq)
                    then desc <> "*"
                    else desc
        errs = fromMaybe [] (hasErrors env (T.pack name))
        cls = if null errs then "row" else "row error"

    lomakeFieldValidate' name = D <$> (lomakeFieldValidate (Proxy :: Proxy a) (Proxy :: Proxy req)  (T.pack name))

    lomakeFieldPretty' x = case srequired (Proxy :: Proxy req) of
        SRequired -> case x of
            D val -> p val
        SOptional -> case x of
            D (Just val) -> p val
            D Nothing    -> (desc, EmptyField)
      where
        p :: a -> (Text, Field)
        p val = (desc, lomakeFieldPretty val)

        proxySym = Proxy :: Proxy sym
        desc = T.pack $ symbolVal proxySym

-------------------------------------------------------------------------------
-- SOP magic
-------------------------------------------------------------------------------

sopView
    :: forall a xs m. (HasDatatypeInfo a, Code a ~ '[xs], All LomakeField' xs, Monad m)
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
    => a -> [(Text, Field)]
sopPretty x = case from x of
    (SOP (Z xs)) -> hcollapse $
        hcmap (Proxy :: Proxy LomakeField') (K . lomakeFieldPretty' . unI) xs
    _            -> error "sopPretty: impossible happened"

-------------------------------------------------------------------------------
-- LomakeValidate
-------------------------------------------------------------------------------

-- | Lomake forms are 'Applicative'.
newtype LomakeValidate a = LomakeValidate ([(Text, Text)] -> [MP.FileData MP.Mem] -> (Map Text [Text], Maybe a))

data LomakeResult a = LomakeResult LomakeEnv (Maybe a)

runLomakeValidate :: LomakeValidate a -> [(Text, Text)] -> [MP.FileData MP.Mem] -> LomakeResult a
runLomakeValidate (LomakeValidate f) params files =
    case f params files of
        (errs, x) -> LomakeResult (LomakeEnv errs params files) x

instance Functor LomakeValidate where
    fmap f (LomakeValidate l)  = LomakeValidate $ \xs ys -> fmap (fmap f) (l xs ys)

instance Applicative LomakeValidate where
    pure x = LomakeValidate $ \_ _ -> (Map.empty, Just x)
    LomakeValidate f <*> LomakeValidate x = LomakeValidate $ \p q ->
        case (f p q, x p q) of
            ((errF, valF), (errX, valX)) ->
                (Map.unionWith mappend errF errX, valF <*> valX)

requiredText
    :: Text
    -> LomakeValidate Text
requiredText name = LomakeValidate $ \p _ ->
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
optionalText name = LomakeValidate $ \p _ ->
    (Map.empty, Just $ lookup name p)

-- | Analogue to 'mapMaybe', 'Left' value is a warning issued.
validate :: (a -> Either (Text, Text) b) -> LomakeValidate a -> LomakeValidate b
validate f (LomakeValidate l) = LomakeValidate $ \p q ->
    case l p q of
        (errs, Nothing) -> (errs, Nothing)
        (errs, Just x)  -> case f x of
            Right y     -> (errs, Just y)
            Left (n, m) -> (Map.unionWith mappend errs (Map.singleton n [m]), Nothing)

ovalidate
    :: forall a b req. (SRequiredI req)
    => Proxy req
    -> (a -> Either (Text, Text) b)
    -> LomakeValidate (O req a) -> LomakeValidate (O req b)
ovalidate p f = validate (otraverse p f)


-- | Generalisation of 'requiredText' and 'optionalText'.
lomakeText
    :: forall req. (SRequiredI req)
    => Proxy req
    -> Text
    -> LomakeValidate (O req Text)
lomakeText p = case srequired p of
    SRequired -> requiredText
    SOptional -> optionalText


-------------------------------------------------------------------------------
-- Section D
-------------------------------------------------------------------------------

class LomakeSection a where
    lomakeSectionView :: Monad m => Proxy a -> LomakeEnv -> HtmlT m ()
    default lomakeSectionView
        :: forall xs m. (HasDatatypeInfo a, Code a ~ '[xs], All LomakeField' xs, Monad m)
        => Proxy a -> LomakeEnv -> HtmlT m ()
    lomakeSectionView = sopView

    lomakeSectionForm :: LomakeValidate a
    default lomakeSectionForm
        :: forall xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs],All LomakeField' xs)
        => LomakeValidate a
    lomakeSectionForm = sopForm

    lomakeSectionPretty :: a -> [(Text, Field)]
    default lomakeSectionPretty
        :: forall xs. (Generic a, HasDatatypeInfo a, Code a ~ '[xs],  All LomakeField' xs)
        => a -> [(Text, Field)]
    lomakeSectionPretty = sopPretty

class LomakeSection' a where
    lomakeSectionView' :: Monad m => Proxy a -> LomakeEnv -> HtmlT m ()
    lomakeSectionForm' :: LomakeValidate a
    lomakeSectionPretty' :: a -> Section

instance (LomakeSection a, KnownSymbol sym) => LomakeSection' (D sym 'Required a) where
    lomakeSectionView' _ env = do
        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
            h2_ $ fromString $ symbolVal (Proxy :: Proxy sym)
        lomakeSectionView (Proxy :: Proxy a) env

    lomakeSectionForm' = D <$> lomakeSectionForm

    lomakeSectionPretty' (D x) = Section
        { _secName = T.pack $ symbolVal (Proxy :: Proxy sym)
        , _secFields = lomakeSectionPretty x
        }

sopFormView
    :: forall a xs m. (Code a ~ '[xs], All LomakeSection' xs, Monad m)
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
    => a -> [Section]
sopFormPretty x = case from x of
    SOP (Z xs) -> hcollapse $
        hcmap (Proxy :: Proxy LomakeSection') (K . lomakeSectionPretty' . unI) xs
    _          -> error "sopFormPretty: impossible happened"

-------------------------------------------------------------------------------
-- LomakeSection F
-------------------------------------------------------------------------------

instance KnownSymbol sym => LomakeField' (F sym) where
    lomakeFieldView' _ env name = div_ [class_ cls] $ do
        div_ [class_ "large-4 columns"] $
            label_ [class_ "text-right middle-text-left middle", Lucid.for_ $ T.pack name] $ do
                toFancyHtml desc'
                {-
                when (not $ T.null extra) $ do
                    br_ []
                    small_ $ toHtml extra
                -}
                F.for_ errs $ \e -> do
                    br_ []
                    span_ $ toHtml e
        div_ [class_ "large-8 columns"] $
            input_ [type_ "file", name_ $ T.pack name ]
      where
        proxySym = Proxy :: Proxy sym
        desc = T.pack (symbolVal proxySym)
        desc' = if True -- always required
                    then desc <> "*"
                    else desc
        errs = fromMaybe [] (hasErrors env (T.pack name))
        cls = if null errs then "row" else "row error"

    lomakeFieldValidate' name = LomakeValidate $ \ps qs ->
        case asum $ fromFiles qs ++ fromFields ps of
            Success f    -> (Map.empty, Just f)
            Failure errs -> (Map.singleton (T.pack name) errs', Nothing)
              where
                errs' | null errs = ["Pakollinen kenttä"]
                      | otherwise = errs
      where
        fromFiles qs =
            [ if ctype == "application/pdf"
              then checkSize bs
              else Failure ["Vain PDF tiedostot"]
            | MP.FileData name' _ ctype lbs <- qs
            , T.pack name == name'
            , let bs = LBS.toStrict lbs
            ]

        fromFields ps =
            [ if BS.null (snd decoded)
              then checkSize $ fst decoded
              else Failure ["Jotain meni vikaan, yritä latamaan uudelleen"]
            | (name', da) <- ps
            , T.pack name == name'
            , let decoded = Base16.decode $ TE.encodeUtf8 da
            , BS.null (snd decoded)
            ]

        checkSize bs =
            if BS.length bs <= 500000
            then Success $ F bs
            else Failure ["Tiedosto on liian iso"]


    lomakeFieldPretty' (F bs) = (desc, FileField bs)
      where
        proxySym = Proxy :: Proxy sym
        desc = T.pack $ symbolVal proxySym

-------------------------------------------------------------------------------
-- Form
-------------------------------------------------------------------------------

class LomakeForm a where
    lomakeView   :: Monad m => Proxy a -> LomakeEnv -> HtmlT m ()
    default lomakeView
        :: forall xs m. (Code a ~ '[xs], All LomakeSection' xs, Monad m)
        => Proxy a -> LomakeEnv -> HtmlT m ()
    lomakeView = sopFormView

    lomakeValidate  :: LomakeValidate a
    default lomakeValidate
        :: forall xs. (Generic a, Code a ~ '[xs], All LomakeSection' xs)
        => LomakeValidate a
    lomakeValidate = sopFormValidate

    lomakePretty    :: a -> [Section]
    default lomakePretty
        :: forall xs. (Generic a, Code a ~ '[xs],  All LomakeSection' xs)
        => a -> [Section]
    lomakePretty = sopFormPretty

instance LomakeForm a => Form.FromForm (LomakeResult a) where
    fromForm f = Right $ runLomakeValidate lomakeValidate (legacy (Form.unForm f)) []
      where
        legacy hm = [ (k, v) | (k, vs) <- HM.toList hm, v <- vs ]

instance LomakeForm a => MP.FromMultipart MP.Mem (LomakeResult a) where
    fromMultipart (MP.MultipartData fs fd) = Just $
        runLomakeValidate lomakeValidate
            [ (k, v)
            | MP.Input k v <- fs
            ]
            fd

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

instance LomakeField Text where
    lomakeFieldPretty = ShortField
    lomakeFieldView _ env name =
        input_ [type_ "text", name_  name, value_ $ submittedTextValue env name]
    lomakeFieldValidate _ = lomakeText

-------------------------------------------------------------------------------
-- Bool
-------------------------------------------------------------------------------

instance HumanShow Bool where
    humanShow = bool "Ei" "Kyllä"

instance LomakeEnum Bool where
    universe = [True, False]

instance LomakeField Bool where
    lomakeFieldView = enumLomakeFieldView
    lomakeFieldValidate = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Enum
-------------------------------------------------------------------------------

enumLomakeFieldValidate
    :: forall a req. (Enum a, Bounded a, Show a, SRequiredI req)
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
-- Fancy text
-------------------------------------------------------------------------------

toFancyHtml :: Monad m => Text -> HtmlT m ()
toFancyHtml = go False . T.splitOn "*" where
    go _ [] = pure ()
    go _ [t] | T.null t = toHtml ("*" :: String)
    go False (t:ts) = toHtml t      >> go True ts
    go True  (t:ts) = i_ (toHtml t) >> go False ts
