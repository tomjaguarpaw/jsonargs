{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set
import           Control.Applicative (liftA2, (<|>), empty)

data OneField a = OneField (Data.Set.Set Text) (HM.HashMap Text A.Value -> Maybe a)
data AllField a = AllField (Data.Set.Set Text) (HM.HashMap Text A.Value -> Maybe a)
                                          
newtype Parser a = Parser { runParser :: A.Value -> Maybe a }

oneField :: Text -> Parser a -> OneField a
oneField s p = OneField (Data.Set.singleton s) $ \hm -> HM.lookup s hm >>= parse p

(<||>) :: OneField a -> OneField a -> OneField a
OneField s1 f1 <||> OneField s2 f2 = OneField (s1 `mappend` s2) (liftA2 (<|>) f1 f2)

noField :: OneField a
noField = OneField mempty (const empty)

oneOf :: [OneField a] -> Parser a
oneOf oneFields = Parser $ \case
  A.Object hm -> let keys = HM.keys hm
                 in case keys of
                     [key] -> if key `Data.Set.member` s
                              then p hm
                              else Nothing
                     _ -> Nothing

  where OneField s p = foldr (<||>) noField oneFields

allField :: Text -> Parser a -> AllField a
allField s p = AllField (Data.Set.singleton s) $ \hm -> HM.lookup s hm >>= parse p

allOf :: AllField a -> Parser a
allOf (AllField s p) = Parser $ \case
  A.Object hm -> if s == Data.Set.fromList (HM.keys hm)
                 then p hm
                 else Nothing
  _ -> Nothing

int :: Parser Scientific
int = Parser $ \case
  A.Number s -> Just s
  _ -> Nothing

string :: Parser Text
string = Parser $ \case
  A.String s -> Just s
  _ -> Nothing

instance Functor AllField where
  fmap f (AllField s p) = AllField s ((fmap . fmap) f p)

instance Applicative AllField where
  pure = AllField mempty . pure . pure
  AllField mf f <*> AllField mx x = AllField (mf `mappend` mx) (liftA2 (<*>) f x)

instance Functor OneField where
  fmap f (OneField s p) = OneField s ((fmap . fmap) f p)

instance Functor Parser where
  fmap f (Parser g) = Parser ((fmap . fmap) f g)

instance Applicative Parser where
  pure = Parser . pure . pure
  Parser f <*> Parser x = Parser (liftA2 (<*>) f x)

data ComputeTarget = GPU Scientific Text | CPU Scientific deriving Eq

computeTarget = oneOf [ oneField "gpu" gpu
                      , oneField "cpu" cpu
                      ]


gpu = allOf (GPU <$> allField "gpu_id" gpu_id
                 <*> allField "cluster" cluster)

cpu = allOf (fmap CPU $ allField "num_cpus" num_cpus)

num_cpus :: Parser Scientific
num_cpus = int
gpu_id = int
cluster = string

size = oneOf [ oneField "large" int
             , oneField "small" int
             ]

parse :: Parser a -> A.Value -> Maybe a
parse (Parser f) = f

main :: IO ()
main = do
  assert $ parse int (A.Number 1) == Just 1

  assert $ parse cpu (A.Object (HM.fromList [("num_cpus", A.Number 2)])) == Just (CPU 2)
  assert $ parse cpu (A.Object (HM.fromList [("num_cpus", A.Number 2)
                                            ,("other", A.Number 0)
                                            ])) == Nothing
  assert $ parse cpu (A.Object (HM.fromList [])) == Nothing

  assert $ parse size (A.Object (HM.fromList [("large", A.Number 3)])) == Just 3


  putStrLn ""
  putStrLn ""
  putStrLn "Success!"
  putStrLn ""
  putStrLn ""

  where assert b = if b then return () else error "Assert failed"
