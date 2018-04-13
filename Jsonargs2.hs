{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set
import           Data.Scientific (Scientific)

data FunctorW f a where
  FunctorW :: f a -> FunctorW f a
  Fmap :: (a -> b) -> FunctorW f a -> FunctorW f b

data Schema a where
  SMap    :: (a -> b) -> Schema a -> Schema b
  SString :: Maybe Text -> Schema Text
  SNumber :: Maybe Scientific -> Schema Scientific
  SOneOf  :: SchemaOneOf a -> Schema a
  SAllOf  :: SchemaAllOf a -> Schema a


data SchemaOneOf a where
  SchemaOneOf :: [(Text, Schema a)] -> SchemaOneOf a
  SchemaOneOfDefault :: (Text, Schema a) -> [(Text, Schema a)] -> SchemaOneOf a

data SchemaAllOf a where
  AllField :: Text -> Schema a -> SchemaAllOf a
  Apply :: SchemaAllOf (a -> b) -> SchemaAllOf a -> SchemaAllOf b
  Pure :: a -> SchemaAllOf a



merge :: Schema a -> Maybe A.Value -> Maybe a
merge = \case
    SMap f s'     -> fmap f . merge s'
    SString mText -> \case
      Nothing -> mText
      Just (A.String t) -> Just t
      _          -> Nothing
    SNumber mNumber -> \case
      Nothing -> mNumber
      Just (A.Number n) -> Just n
      _          -> Nothing
    SOneOf s' -> mergeSchemaOneOf s'
    SAllOf s' -> mergeSchemaAllOf s'

mergeSchemaOneOf :: SchemaOneOf a -> Maybe A.Value -> Maybe a
mergeSchemaOneOf = \case
  SchemaOneOf l -> \case
    Nothing -> Nothing
    Just a  -> case a of
      A.Object hm -> case HM.toList hm of
        []      -> Nothing
        (_:_:_) -> Nothing
        [(field, fieldOther)] -> do
          schema <- lookup field l
          merge schema (Just fieldOther)
      _ -> Nothing
  SchemaOneOfDefault (defField, defSchema) l -> \case
    Nothing -> merge defSchema Nothing
    Just a -> case a of
      A.Object hm -> case HM.toList hm of
        [] -> merge defSchema Nothing
        (_:_:_) -> Nothing
        [(field, fieldOther)] -> do
          schema <- lookup field ((defField, defSchema) : l)
          merge schema (Just fieldOther)
      _ -> Nothing

mergeSchemaAllOf' :: SchemaAllOf a -> (HM.HashMap Text A.Value -> Maybe a, [Text])
mergeSchemaAllOf' = \case
  Pure a -> (const (Just a), [])
  Apply x y -> case mergeSchemaAllOf' x of
    (gx, lx) -> case mergeSchemaAllOf' y of
      (gy, ly) -> (\v -> gx v <*> gy v, lx ++ ly)
  AllField t schema -> (merge schema . HM.lookup t, [t])

mergeSchemaAllOf :: SchemaAllOf a -> Maybe A.Value -> Maybe a
mergeSchemaAllOf s = \case
  Nothing -> f HM.empty
  Just (A.Object hm) -> if Data.Set.fromList (HM.keys hm)
                           `Data.Set.isSubsetOf`
                           Data.Set.fromList ts
                        then f hm
                        else Nothing
  Just _ -> Nothing

  where (f, ts) = mergeSchemaAllOf' s


data ComputeTarget = GPU Scientific Text | CPU Scientific deriving Eq

computeTarget = SOneOf (SchemaOneOfDefault ("gpu", gpu)
                                           [("cpu", cpu)])

cpu = SAllOf (AllField "num_cpus" (SMap CPU num_cpus))

gpu = SAllOf (Pure GPU `Apply` AllField "gpu_id" gpu_id
                       `Apply` AllField "cluster" cluster)

gpu_id = SNumber (Just 0)
cluster = SString (Just "local")
num_cpus = SNumber (Just 1)


data Size = Large | Small deriving Eq

int0 = SNumber (Just 0)

size = SOneOf (SchemaOneOfDefault ("large", SMap (const Large) int0)
                                  [ ("small", SMap (const Small) int0) ])

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
