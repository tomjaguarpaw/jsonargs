{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

import Jsonargs2 (FunctorW(FunctorW),
                  ApplicativeW(ApplicativeW), SumW(Sum, SumW), Sum(..),
                  onApplicativeW, onSumW, onFunctorW,
                  Size(..),
                  assert)
import Control.Applicative (liftA2)

import qualified Data.Map

type Schema = FunctorW SchemaB

data SchemaB a where
  SString :: SchemaB String
  SOneOf  :: OneOf a -> SchemaB a
  SAllOf  :: AllOf a -> SchemaB a

type AllOf = ApplicativeW AllOfB

data AllOfB a where
  AllOfOnce :: String -> Schema a -> AllOfB a
  AllOfMany :: String -> Schema a -> AllOfB [a]

data OneOf a where
  OneOf :: OneOfFields a -> OneOf a

type OneOfFields = SumW OneOfFieldsB

data OneOfFieldsB a where
  OneField :: String -> Schema a -> OneOfFieldsB a

data Opt = Opt String deriving (Ord, Eq)
data Arg = Arg String deriving (Ord, Eq)

data Token = TOpt Opt
           | TArg Arg
  deriving (Ord, Eq)


bin :: (a -> Either b c)
    -> [a]
    -> ([c], Maybe (b, [a]))
bin _ [] = ([], Nothing)
bin f (a:as) = case f a of
  Left b  -> ([], Just (b, as))
  Right c -> let (cs, rest) = bin f as
             in (c:cs, rest)

bin2 :: Ord b
     => (a -> Either b c)
     -> b
     -> [a]
     -> Data.Map.Map b [[c]]
     -> Data.Map.Map b [[c]]
bin2 f b as dm = case bin f as of
  (cs, Nothing) -> Data.Map.alter (Just . \case
                                      Nothing -> [cs]
                                      Just css -> cs:css)
                                  b
                                  dm
  (cs, Just (b', as')) ->
    let dm' = Data.Map.alter (Just . \case
                                 Nothing -> [cs]
                                 Just css -> cs:css)
                             b
                             dm
    in bin2 f b' as' dm'

bin3 :: Ord b
     => (a -> Either b c)
     -> [a]
     -> Either String (Data.Map.Map b [[c]])
bin3 f as = case as of
  [] -> return Data.Map.empty
  (a:as') -> case f a of
    Left b -> return (bin2 f b as' Data.Map.empty)
    Right _ -> Left "Got a c first"

data M a = M (Data.Map.Map String [[Token]] -> Either String a, [String])

instance Functor M where
  fmap f (M (g, ts)) = M ((fmap . fmap) f g, ts)

instance Applicative M where
  pure x = M ((pure . pure) x, mempty)
  M (ff, fts) <*> M (xf, xts) = M (liftA2 (<*>) ff xf, fts `mappend` xts)

parse ::Schema a -> [Token] -> Either String a
parse schema tokens = onFunctorW (parseB tokens) schema

parseB :: [Token] -> SchemaB a -> Either String a
parseB tokens = \case
  SString -> parseString tokens
  SOneOf oneOf' -> parseOneOf oneOf' tokens
  SAllOf allOf' -> parseAllOf allOf' tokens

parseAllOfB :: AllOfB a -> M a
parseAllOfB = \case
  AllOfOnce field schema ->
    M (\m -> case Data.Map.lookup field m of
          Nothing -> Left ("Expected " ++ field)
          Just [] -> error "My brain exploded -- didn't get any tokens"
          Just [tokens] -> parse schema tokens
          Just (_:_:_) -> Left ("Got too many instances of " ++ field),
       [field])
  AllOfMany field schema ->
    M (\m -> case Data.Map.lookup field m of
          Nothing -> return []
          Just [] -> error "My brain exploded -- didn't get any tokens"
          Just tokens -> traverse (parse schema) tokens,
       [field])

parseAllOf :: AllOf a -> [Token] -> Either String a
parseAllOf allOf tokens = do
  binnedFields <- case eBinnedFields of
    Left _ -> Left ("Was expecting an option but got an argument "
                     ++ "or an option I didn't expect")
    Right r -> return r
  parseObject binnedFields
  -- TODO: We should check that two AllOfs are not parsing the same
  -- field name
  where M (parseObject, fields) = onApplicativeW parseAllOfB allOf
        choose = \case
          TArg a -> Right (TArg a)
          TOpt (Opt o) -> if o `elem` fields
                          then Left o
                          else Right (TOpt (Opt o))
        eBinnedFields = bin3 choose tokens

data SSList a = SSList { unSSList :: [(String, Schema a)] }

instance Sum SSList where
  sZero = SSList []
  sSum  = SSList . concat . map unSSList

parseOneOf :: OneOf a -> [Token] -> Either String a
parseOneOf (OneOf oneOf') tokens = case tokens of
  [] -> Left "Nothing provided for a OneOf"
  (TArg _):_ -> Left "Arg provided but expected option for a OneOf"
  (TOpt (Opt o)):rest -> case lookup o fields_schemas of
    Nothing -> Left (o ++ " is not one of the options I can parse")
    Just schema -> parse schema rest
  where fields_schemas = unSSList (onSumW (\case
          OneField field schema -> SSList [(field, schema)]) oneOf')
        -- TODO: We should check that two OneOfs are not parsing the same
        -- field name

parseString :: [Token] -> Either String String
parseString = \case
  [TArg (Arg a)] -> Right a
  [] -> Left "Expected something but found nothing"
  (TOpt _):_ -> Left "Expected a string but found an option"
  (TArg _):_:_ -> Left "Had unexpected leftovers"
  
name :: Schema String
name = string

string :: Schema String
string = FunctorW SString

nothing :: Schema ()
nothing = FunctorW (SAllOf (pure ()))

oneOf :: [(String, Schema a)] -> Schema a
oneOf =
  FunctorW
  . SOneOf
  . OneOf
  . Sum
  . map (\(a, b) -> SumW (OneField a b))

large :: Schema Size
large =  oneOf [ ("large", fmap (const Large) nothing)
               , ("small", fmap (const Small) nothing)
               ]

login :: Schema (String, String)
login = FunctorW (SAllOf ((,) <$> ApplicativeW (AllOfOnce "username" string)
                              <*> ApplicativeW (AllOfOnce "password" string)))

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

main :: IO ()
main = do
  assert $ isLeft (parse name [TOpt (Opt "foo")])
  assert $ isLeft (parse name [])
  assert $ parse name [TArg (Arg "foo")] == Right "foo"

  assert $ parse nothing [] == Right ()
  assert $ isLeft (parse nothing [TOpt (Opt "foo")])
  assert $ isLeft (parse nothing [TArg (Arg "foo")])
  
  assert $ isLeft (parse large [])
  assert $ isLeft (parse large [TOpt (Opt "foo")])
  assert $ isLeft (parse large [TArg (Arg "foo")])
  assert $ parse large [TOpt (Opt "large")] == Right Large
  assert $ parse large [TOpt (Opt "small")] == Right Small
  
  assert $ isLeft (parse login [])
  assert $ isLeft (parse login [TOpt (Opt "foo")])
  assert $ isLeft (parse login [TArg (Arg "foo")])
  assert $ isLeft (parse login [TOpt (Opt "username")])
  assert $ isLeft (parse login [TOpt (Opt "username"), TArg (Arg "foo")])
  assert $ parse login [TOpt (Opt "username"), TArg (Arg "foo")
                       ,TOpt (Opt "password"), TArg (Arg "bar")]
              == Right ("foo", "bar")
  assert $ parse login [TOpt (Opt "password"), TArg (Arg "baz")
                       ,TOpt (Opt "username"), TArg (Arg "quux")]
              == Right ("quux", "baz")
