{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.HashMap.Strict as HM
import qualified Data.Set
import           Data.Scientific (Scientific)
import           Control.Applicative (liftA2, Const(Const), getConst, empty)
import           Data.Monoid ((<>))
import           Data.Foldable (traverse_, asum)

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

data FunctorW f a where
  FunctorW :: f a -> FunctorW f a
  Fmap :: (a -> b) -> FunctorW f a -> FunctorW f b

data ApplicativeW f a where
  ApplicativeW :: f a -> ApplicativeW f a
  Pure :: a -> ApplicativeW f a
  Apply :: ApplicativeW f (a -> b) -> ApplicativeW f a -> ApplicativeW f b

data SumW f a where
  SumW :: f a -> SumW f a
  Sum :: [SumW f a] -> SumW f a
  Zero :: SumW f a

data SchemaB a where
  SString :: Maybe Text -> SchemaB Text
  SNumber :: Maybe Scientific -> SchemaB Scientific
  SOneOf  :: OneOf a -> SchemaB a
  SAllOf  :: AllOf a -> SchemaB a

type Schema = FunctorW SchemaB

instance Functor (FunctorW f) where
  fmap = Fmap

instance Functor (ApplicativeW f) where
  fmap f x = pure f <*> x

instance Applicative (ApplicativeW f) where
  pure = Pure
  (<*>) = Apply

instance Sum (SumW f) where
  sZero = Zero
  sSum = Sum

data OneOfFieldsB a where
  OneField :: (Text, Schema a) -> OneOfFieldsB a

type OneOfFields = SumW OneOfFieldsB

data OneOf a where
  OneOf :: OneOfFields a -> OneOf a
  OneOfDefault :: (Text, Schema a) -> OneOfFields a -> OneOf a

type AllOf = ApplicativeW AllOfB

data AllOfB a where
  AllField :: Text -> Schema a -> AllOfB a

onFunctorW :: Functor g => (forall a. f a -> g a) -> (FunctorW f b -> g b)
onFunctorW f = \case
  FunctorW b -> f b
  Fmap g fw -> fmap g (onFunctorW f fw)

onApplicativeW :: Applicative g
               => (forall a. f a -> g a)
               -> (ApplicativeW f b -> g b)
onApplicativeW f = \case
  ApplicativeW b -> f b
  Pure x -> pure x
  g `Apply` x -> onApplicativeW f g <*> onApplicativeW f x

onSumW :: Sum g
       => (forall a. f a -> g a)
       -> SumW f b -> g b
onSumW f = \case
  SumW b -> f b
  Sum bs -> sSum (map (onSumW f) bs)
  Zero   -> sZero

merge :: Schema a -> Maybe A.Value -> Maybe a
merge = flip $ \mv -> onFunctorW $ \case
    SString mText -> case mv of
      Nothing -> mText
      Just (A.String t) -> Just t
      _          -> Nothing
    SNumber mNumber -> case mv of
      Nothing -> mNumber
      Just (A.Number n) -> Just n
      _          -> Nothing
    SOneOf s' -> mergeOneOf s' mv
    SAllOf s' -> mergeAllOf s' mv

mergeOneOf :: OneOf a -> Maybe A.Value -> Maybe a
mergeOneOf oneOf =
  let (def, l) = case oneOf of
        OneOf ofs -> (Nothing, oneFields ofs)
        OneOfDefault (defField, defSchema) ofs ->
          (merge defSchema Nothing,
            ((defField, defSchema) : oneFields ofs))
    in cont def l

  where cont def l = \case
          Nothing -> def
          Just a  -> case a of
            A.Object hm -> case HM.toList hm of
              []      -> def
              (_:_:_) -> Nothing
              [(field, fieldOther)] -> do
                schema <- lookup field l
                merge schema (Just fieldOther)
            _ -> Nothing

oneFields :: OneOfFields a -> [(Text, Schema a)]
oneFields = unTSList . onSumW (\case
  OneField field -> TSList [field])

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
mergeAllOf s = \case
  Nothing -> f HM.empty
  Just (A.Object hm) -> if Data.Set.fromList (HM.keys hm)
                           `Data.Set.isSubsetOf`
                           Data.Set.fromList ts
                        then f hm
                        else Nothing
  Just _ -> Nothing

  where M (f, ts) = mergeAllOf'' s


data ComputeTarget = GPU Scientific Text | CPU Scientific | TPU Text
                   deriving Eq


mapConst :: (a -> c) -> Const a b -> Const c b
mapConst f (Const a) = Const (f a)

help :: Schema a -> [Text]
help = getConst . help'

help' :: Schema a -> Const [Text] a
help' = onFunctorW $ \case
  SString mt -> Const (["<string>" <> maybe "" (\t -> " (default is \"" <> t <> "\")") mt])
  SNumber mn -> Const (["<number>" <> maybe "" (\t -> " (default is " <> Data.Text.pack (show t) <> ")") mn])
  SOneOf x   -> Const ["One of"] *> helpOneOf x
  SAllOf x   -> Const ["All of"] *> helpAllOf x

helpAllOf :: AllOf a -> Const [Text] a
helpAllOf = onApplicativeW $ \case
  AllField field schema -> helpFieldSchema "" (field, schema)

helpFieldSchema :: Text -> (Text, Schema a) -> Const [Text] a
helpFieldSchema extra (field, schema) =
  Const ([ "\"" <> field <> "\":" <> extra])
  *> mapConst (map ((Data.Text.pack "    ") <>)) (help' schema)

helpOneOfFields :: Text -> OneOfFields a -> Const [Text] a
helpOneOfFields extra = onSumW $ \case
  OneField oneField -> helpFieldSchema extra oneField

helpOneOf :: OneOf a -> Const [Text] a
helpOneOf = \case
  OneOf ofs -> helpOneOfFields "" ofs
  OneOfDefault defaults_ ofs -> sSum
    [ helpFieldSchema " (default)" defaults_
    , helpOneOfFields "" ofs
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
