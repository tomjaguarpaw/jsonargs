{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set
import           Data.Scientific (Scientific)
import           Control.Applicative (liftA2)

data FunctorW f a where
  FunctorW :: f a -> FunctorW f a
  Fmap :: (a -> b) -> FunctorW f a -> FunctorW f b

data ApplicativeW f a where
  ApplicativeW :: f a -> ApplicativeW f a
  Pure :: a -> ApplicativeW f a
  Apply :: ApplicativeW f (a -> b) -> ApplicativeW f a -> ApplicativeW f b

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

data OneOf a where
  OneOf :: [(Text, Schema a)] -> OneOf a
  OneOfDefault :: (Text, Schema a) -> [(Text, Schema a)] -> OneOf a

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
mergeOneOf = \case
  OneOf l -> \case
    Nothing -> Nothing
    Just a  -> case a of
      A.Object hm -> case HM.toList hm of
        []      -> Nothing
        (_:_:_) -> Nothing
        [(field, fieldOther)] -> do
          schema <- lookup field l
          merge schema (Just fieldOther)
      _ -> Nothing
  OneOfDefault (defField, defSchema) l -> \case
    Nothing -> merge defSchema Nothing
    Just a -> case a of
      A.Object hm -> case HM.toList hm of
        [] -> merge defSchema Nothing
        (_:_:_) -> Nothing
        [(field, fieldOther)] -> do
          schema <- lookup field ((defField, defSchema) : l)
          merge schema (Just fieldOther)
      _ -> Nothing

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


data ComputeTarget = GPU Scientific Text | CPU Scientific deriving Eq

oneOfDefault :: (Text, Schema a) -> [(Text, Schema a)] -> Schema a
oneOfDefault t ts = FunctorW (SOneOf (OneOfDefault t ts))

allField :: Text -> Schema a -> AllOf a
allField t = ApplicativeW . AllField t

allOf :: AllOf a -> Schema a
allOf = FunctorW . SAllOf

number :: Maybe Scientific -> Schema Scientific
number = FunctorW . SNumber

string :: Maybe Text -> Schema Text
string = FunctorW . SString


computeTarget = oneOfDefault ("gpu", gpu) [("cpu", cpu)]

cpu = allOf (allField "num_cpus" (fmap CPU num_cpus))

gpu = allOf (GPU <$> allField "gpu_id" gpu_id
                 <*> allField "cluster" cluster)

gpu_id = number (Just 0)
cluster = string (Just "local")
num_cpus = number (Just 1)


data Size = Large | Small deriving Eq

int0 = number (Just 0)

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
                    , (Just (d ["gpu" .= d []]), GPU 0 "local")
                    , (Just (d ["gpu" .= d ["gpu_id" .= n 1]]), GPU 1 "local")
                    , (Just (d ["cpu" .= d []]), CPU 1)
                    , (Just (d ["cpu" .= d ["num_cpus" .= n 5]]), CPU 5)
                    ]

  flip mapM_ ct_defaults $ \(ct_default, ct_expected) -> assert $ merge computeTarget ct_default == Just ct_expected

  putStrLn ""
  putStrLn ""
  putStrLn "Success!"
  putStrLn ""
  putStrLn ""

  where assert b = if b then return () else error "Assertion failure"
