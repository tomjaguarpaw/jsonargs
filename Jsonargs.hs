{-# LANGUAGE LambdaCase #-}

import qualified Data.Aeson as A
import           Data.Scientific (Scientific)

data OneField a
data AllField a
data Parser a = Parser (A.Value -> Maybe a)

oneField :: String -> Parser a -> OneField a
oneField = undefined

oneOf :: [OneField a] -> Parser a
oneOf = undefined

allField :: String -> Parser a -> AllField a
allField = undefined

allOf :: AllField a -> Parser a
allOf = undefined

int :: Parser Scientific
int = Parser $ \case
  A.Number s -> Just s

string :: Parser String
string = undefined

instance Functor AllField where
instance Functor OneField where
instance Applicative AllField where

data ComputeTarget = GPU Scientific String | CPU Scientific

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

parse :: Parser a -> A.Value -> Maybe a
parse (Parser f) = f

main :: IO ()
main = do
  assert $ parse int (A.Number 1) == Just 1

  putStrLn ""
  putStrLn ""
  putStrLn "Success!"
  putStrLn ""
  putStrLn ""

  where assert b = if b then return () else error "Assert failed"
  


