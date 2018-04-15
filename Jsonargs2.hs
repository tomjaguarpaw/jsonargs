{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Jsonargs2 where

import           Control.Category ((>>>))
import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.HashMap.Strict as HM
import qualified Data.Set
import           Data.Scientific (Scientific)
import           Control.Applicative (liftA2, Const(Const), getConst)
import           Data.Monoid ((<>))

-- { New class

class Sum f where
  sZero :: f a
  sSum :: [f a] -> f a

instance Monoid m => Sum (Const m) where
  sZero = Const mempty
  sSum = Const . mconcat . map getConst

newtype TSList a = TSList { unTSList :: [(Text, Schema a)] }

instance Sum TSList where
  sZero = TSList []
  sSum  = TSList . concat . map unTSList

-- }

-- { Free structures

data FunctorW f a where
  FunctorW :: f a -> FunctorW f a
  Fmap :: (a -> b) -> FunctorW f a -> FunctorW f b

instance Functor (FunctorW f) where
  fmap = Fmap

onFunctorW :: Functor g => (forall a. f a -> g a) -> (FunctorW f b -> g b)
onFunctorW f = \case
  FunctorW b -> f b
  Fmap g fw -> fmap g (onFunctorW f fw)

data ApplicativeW f a where
  ApplicativeW :: f a -> ApplicativeW f a
  Pure :: a -> ApplicativeW f a
  Apply :: ApplicativeW f (a -> b) -> ApplicativeW f a -> ApplicativeW f b

instance Functor (ApplicativeW f) where
  fmap f x = pure f <*> x

instance Applicative (ApplicativeW f) where
  pure = Pure
  (<*>) = Apply

onApplicativeW :: Applicative g
               => (forall a. f a -> g a)
               -> (ApplicativeW f b -> g b)
onApplicativeW f = \case
  ApplicativeW b -> f b
  Pure x -> pure x
  g `Apply` x -> onApplicativeW f g <*> onApplicativeW f x

data SumW f a where
  SumW :: f a -> SumW f a
  Sum :: [SumW f a] -> SumW f a
  Zero :: SumW f a

instance Sum (SumW f) where
  sZero = Zero
  sSum = Sum

onSumW :: Sum g
       => (forall a. f a -> g a)
       -> SumW f b -> g b
onSumW f = \case
  SumW b -> f b
  Sum bs -> sSum (map (onSumW f) bs)
  Zero   -> sZero

-- }

-- { My base types

data SchemaB a where
  SString :: Maybe Text -> SchemaB Text
  SNumber :: Maybe Scientific -> SchemaB Scientific
  SOneOf  :: OneOf a -> SchemaB a
  SAllOf  :: AllOf a -> SchemaB a

type Schema = FunctorW SchemaB

data OneOfFieldsB a where
  OneField :: (Text, Schema a) -> OneOfFieldsB a

type OneOfFields = SumW OneOfFieldsB

data OneOf a where
  OneOf :: OneOfFields a -> OneOf a
  OneOfDefault :: (Text, Schema a) -> OneOfFields a -> OneOf a

type AllOf = ApplicativeW AllOfB

data AllOfB a where
  AllField :: Text -> Schema a -> AllOfB a

-- }

merge :: Schema a -> Maybe A.Value -> Maybe a
merge = flip $ \mValue -> onFunctorW $ \case
    SString default' -> case mValue of
      Nothing           -> default'
      Just (A.String t) -> Just t
      Just _            -> Nothing
    SNumber default' -> case mValue of
      Nothing           -> default'
      Just (A.Number n) -> Just n
      Just _            -> Nothing
    SOneOf oneOf' -> mergeOneOf oneOf' mValue
    SAllOf allOf' -> mergeAllOf allOf' mValue

mergeOneOf :: OneOf a -> Maybe A.Value -> Maybe a
mergeOneOf oneOf = \case
  Nothing -> default'
  Just a  -> case a of
    A.Object hm -> case fields hm of
      None -> default'
      Many -> Nothing
      One field fieldOther -> do
        schema <- lookup field (oneOfFields oneOf)
        merge schema (Just fieldOther)
    _ -> Nothing

  where default' = case oneOf of
          OneOf _ -> Nothing
          OneOfDefault (_, defSchema) _ ->
            merge defSchema Nothing

        oneOfFields :: OneOf a -> [(Text, Schema a)]
        oneOfFields = \case
          OneOf ofs                 -> oneFields ofs
          OneOfDefault default' ofs -> default' : oneFields ofs

        oneFields :: OneOfFields a -> [(Text, Schema a)]
        oneFields = unTSList . onSumW (\case
           OneField field -> TSList [field])

data Fields k v = None | One k v | Many

fields :: HM.HashMap k v -> Fields k v
fields = HM.toList >>> \case
  [] -> None
  [(k, v)] -> One k v
  (_:_:_) -> Many

data M a = M (HM.HashMap Text A.Value -> Maybe a, [Text])

instance Functor M where
  fmap f (M (g, ts)) = M ((fmap . fmap) f g, ts)

instance Applicative M where
  pure x = M ((pure . pure) x, mempty)
  M (ff, fts) <*> M (xf, xts) = M (liftA2 (<*>) ff xf, fts `mappend` xts)

mergeAllOf'' :: AllOf a -> M a
mergeAllOf'' = onApplicativeW $ \case
  AllField t schema -> M (merge schema . HM.lookup t, [t])

mergeAllOf :: AllOf a -> Maybe A.Value -> Maybe a
mergeAllOf allOf' = \case
  Nothing -> mergeObject HM.empty
  Just (A.Object object) ->
    if objectKeys `Data.Set.isSubsetOf` schemaKeys
    then mergeObject object
    else Nothing
    where objectKeys = Data.Set.fromList (HM.keys object)
          schemaKeys = Data.Set.fromList schemaKeysList
  Just _ -> Nothing

  where M (mergeObject, schemaKeysList) = mergeAllOf'' allOf'


data ComputeTarget = GPU Scientific Text | CPU Scientific | TPU Text
                   deriving Eq

mapConst :: (a -> c) -> Const a b -> Const c b
mapConst f (Const a) = Const (f a)

help :: Schema a -> [Text]
help = getConst . helpC

helpC :: Schema a -> Const [Text] a
helpC = onFunctorW $ \case
  SString mt -> Const ["<string>" <> defaultIs (\t -> "\"" <> t <> "\"") mt]
  SNumber mn -> Const ["<number>" <> defaultIs (Data.Text.pack . show) mn]
  SOneOf x   -> Const ["One of"] *> helpOneOf x
  SAllOf x   -> Const ["All of"] *> helpAllOf x
  where defaultIs f = maybe "" (\t -> " (default is " <> f t <> ")")

helpAllOf :: AllOf a -> Const [Text] a
helpAllOf = onApplicativeW $ \case
  AllField field schema -> helpFieldSchema "" (field, schema)

helpFieldSchema :: Text -> (Text, Schema a) -> Const [Text] a
helpFieldSchema extra (field, schema) =
  Const ([ "\"" <> field <> "\":" <> extra])
  *> (mapConst . map) (indent 4) (helpC schema)
  where indent n = (Data.Text.pack (replicate n ' ') <>)

helpOneOfFields :: Text -> OneOfFields a -> Const [Text] a
helpOneOfFields extra = onSumW $ \case
  OneField oneField -> helpFieldSchema extra oneField

helpOneOf :: OneOf a -> Const [Text] a
helpOneOf = \case
  OneOf oneOfFields -> helpOneOfFields "" oneOfFields
  OneOfDefault defaultField oneOfFields -> sSum
    [ helpFieldSchema " (default)" defaultField
    , helpOneOfFields "" oneOfFields
    ]

oneOfDefault :: (Text, Schema a) -> [(Text, Schema a)] -> Schema a
oneOfDefault def =
  FunctorW
  . SOneOf
  . OneOfDefault def
  . sSum
  . map (SumW . OneField)

allField :: Text -> Schema a -> AllOf a
allField t = ApplicativeW . AllField t

allOf :: AllOf a -> Schema a
allOf = FunctorW . SAllOf

number :: Maybe Scientific -> Schema Scientific
number = FunctorW . SNumber

string :: Maybe Text -> Schema Text
string = FunctorW . SString


required :: Maybe a
required = Nothing

default_ :: a -> Maybe a
default_ = Just

computeTarget2 :: Schema ComputeTarget
computeTarget2 = oneOfDefault ("gpu", allOf (GPU <$> allField "gpu_id" (number (default_ 0))
                                                 <*> allField "cluster" (string (default_ "local"))))
                              [("cpu", allOf (allField "num_cpus" (CPU <$> number (default_ 1))))
                              ]

computeTarget :: Schema ComputeTarget
computeTarget = oneOfDefault ("gpu", gpu) [("cpu", cpu), ("tpu", tpu)]

tpu :: Schema ComputeTarget
tpu = allOf (allField "tpu_ops" (TPU <$> string required))

cpu :: Schema ComputeTarget
cpu = allOf (allField "num_cpus" (fmap CPU num_cpus))

gpu :: Schema ComputeTarget
gpu = allOf (GPU <$> allField "gpu_id" gpu_id
                 <*> allField "cluster" cluster)

gpu_id :: Schema Scientific
gpu_id = number (Just 0)

cluster :: Schema Text
cluster = string (Just "local")

num_cpus :: Schema Scientific
num_cpus = number (Just 1)

data Size = Large | Small deriving Eq

int0 :: Schema Scientific
int0 = number (Just 0)

size :: Schema Size
size = oneOfDefault ("large", fmap (const Large) int0)
                    [("small", fmap (const Small) int0)]

main :: IO ()
main = do
  let (.=) = (,)
      d = A.object
      n = A.Number
      s = A.String

  assert $ merge num_cpus Nothing == Just 1
  assert $ merge cluster Nothing  == Just "local"
  assert $ merge cpu Nothing == Just (CPU 1)

  assert $ merge num_cpus (Just (n 2)) == Just 2
  assert $ merge cluster (Just (s "remote")) == Just "remote"
  assert $ merge cpu (Just (d ["num_cpus" .= n 2])) == Just (CPU 2)
  assert $ merge cpu (Just (d ["shouldn't exist" .= n 2])) == Nothing

  assert $ merge cpu (Just (d [])) == Just (CPU 1)
  assert $ merge gpu (Just (d ["gpu_id" .= n 5])) == Just (GPU 5 "local")

  assert $ merge size Nothing == Just Large

  let ct_defaults = [ (Nothing, GPU 0 "local")
                    , (Just (d []), GPU 0 "local")
                    , (Just (d ["gpu" .= d []]), GPU 0 "local")
                    , (Just (d ["gpu" .= d ["gpu_id" .= n 1]]), GPU 1 "local")
                    , (Just (d ["cpu" .= d []]), CPU 1)
                    , (Just (d ["cpu" .= d ["num_cpus" .= n 5]]), CPU 5)
                    , (Just (d ["tpu" .= d ["tpu_ops" .= s "Some ops"]]), TPU "Some ops")
                    ]

      ct_none = [ d ["tpu" .= d []]
                ]

  flip mapM_ ct_defaults $ \(ct_default, ct_expected) -> assert $ merge computeTarget ct_default == Just ct_expected
  flip mapM_ ct_none $ \ct_default -> assert $ merge computeTarget (Just ct_default) == Nothing

  mapM_ (putStrLn . Data.Text.unpack) (help computeTarget)

  putStrLn ""
  putStrLn ""
  putStrLn "Success!"
  putStrLn ""
  putStrLn ""

  where assert b = if b then return () else error "Assertion failure"
