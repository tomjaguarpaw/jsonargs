{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Jsonargs3 where

import Jsonargs2 (FunctorW, Free(PureG),
                  ApplicativeW, SumW, Sum(..),
                  onApplicativeW, onSumW, onFunctorW,
                  Size(..),
                  assert)
import Control.Applicative (liftA2)
import qualified System.Environment
import qualified Data.List

import qualified Data.Map
import           Data.Functor ((<$))

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

data Opt = Opt String deriving (Ord, Eq, Show)
data Arg = Arg String deriving (Ord, Eq, Show)

data Token = TOpt Opt
           | TArg Arg
  deriving (Ord, Eq, Show)


splitOn :: (a -> Either c b)
        -> [a]
        -> ([c], Maybe (b, [a]))
splitOn _ [] = ([], Nothing)
splitOn f (a:as) = case f a of
  Right b -> ([], Just (b, as))
  Left c  -> let (cs, rest) = splitOn f as
             in (c:cs, rest)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

bin2 :: Ord b
     => (a -> Either c b)
     -> (b, [a])
     -> Data.Map.Map b [[c]]
     -> Data.Map.Map b [[c]]
bin2 f (b, as) dm = case splitOn f as of
  (cs, rest) -> dm' cs |> case rest of
    Nothing -> id
    Just t -> bin2 f t
  where dm' cs = Data.Map.alter (Just . \case
                                    Nothing -> [cs]
                                    Just css -> cs:css)
                                b
                                dm
bin3 :: Ord b
     => (a -> Either c b)
     -> [a]
     -> Either c (Data.Map.Map b [[c]])
bin3 f as = case as of
  [] -> return Data.Map.empty
  (a:as') -> case f a of
    Right b -> return (bin2 f (b, as') Data.Map.empty)
    Left c -> Left c

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
          Nothing -> Left ("Expected --" ++ field)
          Just [] -> error "My brain exploded -- didn't get any tokens"
          Just [tokens] -> parse schema tokens
          Just (_:_:_) -> Left ("Got too many instances of --" ++ field),
       [field])
  AllOfMany field schema ->
    M (\m -> case Data.Map.lookup field m of
          Nothing -> return []
          Just [] -> error "My brain exploded -- didn't get any tokens"
          Just tokens -> traverse (parse schema) tokens,
       [field])

parseAllOf :: AllOf a -> [Token] -> Either String a
parseAllOf allOf' tokens = do
  binnedFields <- case eBinnedFields of
    Left c -> case c of
      TOpt (Opt o) -> Left ("Didn't expect option --" ++ o ++ "\n"
                           ++ iKnowAbout)
      TArg (Arg a) -> Left ("Didn't expect an arg but got " ++ a ++ "\n"
                           ++ iKnowAbout)
    Right r -> return r
  parseObject binnedFields
  -- TODO: We should check that two AllOfs are not parsing the same
  -- field name
  where M (parseObject, fields) = onApplicativeW parseAllOfB allOf'
        choose = \case
          TArg a -> Left (TArg a)
          TOpt (Opt o) -> if o `elem` fields
                          then Right o
                          else Left (TOpt (Opt o))
        eBinnedFields = bin3 choose tokens
        iKnowAbout = "I know about "
                     ++ Data.List.intercalate ", "
                          (map ("--"++) fields)

data SSList a = SSList { unSSList :: [(String, Schema a)] }

instance Sum SSList where
  sZero = SSList []
  sSum  = SSList . concat . map unSSList

parseOneOf :: OneOf a -> [Token] -> Either String a
parseOneOf (OneOf oneOf') tokens = case tokens of
  [] -> Left ("Expected one of "
             ++ Data.List.intercalate ", "
                  (map (("--" ++) . fst) fields_schemas))
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
  [] -> Left "Expected a string but found nothing"
  (TOpt (Opt o)):_ -> Left ("Expected a string but found option --" ++ o)
  (TArg (Arg a)):token:_ -> Left ("Didn't expect to see " ++ s ++ " "
                                 ++ "after " ++ a)
    where s = case token of
            TOpt (Opt o) -> o
            TArg (Arg a) -> a

name :: Schema String
name = string

string :: Schema String
string = PureG SString

nothing :: Schema ()
nothing = PureG (SAllOf (pure ()))

oneOf :: [(String, Schema a)] -> Schema a
oneOf =
  PureG
  . SOneOf
  . OneOf
  . sSum
  . map (\(a, b) -> PureG (OneField a b))

large :: Schema Size
large =  oneOf [ ("large", fmap (const Large) nothing)
               , ("small", fmap (const Small) nothing)
               ]

once :: String -> Schema a -> AllOf a
once field parser = PureG (AllOfOnce field parser)

many :: String -> Schema a -> AllOf [a]
many field parser = PureG (AllOfMany field parser)

allOf :: AllOf a -> Schema a
allOf = PureG . SAllOf

login :: Schema (String, String)
login = allOf ((,) <$> once "username" string
                   <*> once "password" string)

files :: Schema [String]
files = allOf (many "file" string)


multiplex :: Schema (String, [String], [String])
multiplex = allOf ((,,) <$> once "codec" string
                        <*> many "file" string
                        <*> many "output" string)

data Target = X86 | X64
  deriving Show

data Tool = Stack Target
          | Cabal Target Build
  deriving Show

data Build = OldBuild | NewBuild
  deriving Show

data Package = Package String String
  deriving Show

data Install = Install { tool_    :: Tool
                       , packages :: [Package]
                       }
  deriving Show

install :: Schema Install
install = allOf (Install <$> once "tool" tool
                         <*> many "package" package)

tool :: Schema Tool
tool = oneOf [ ("stack", allOf (Stack <$> once "target" target))
             , ("cabal", allOf (Cabal <$> once "target" target
                                      <*> once "build"  build))
             ]

target :: Schema Target
target = oneOf [ ("x86", X86 <$ nothing)
               , ("x64", X64 <$ nothing)
               ]

build :: Schema Build
build = oneOf [ ("old-build", OldBuild <$ nothing)
              , ("new-build", NewBuild <$ nothing)
              ]

package :: Schema Package
package = allOf (Package <$> once "name" string
                         <*> once "version" string)

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

main :: IO ()
main = do
  let o = TOpt . Opt
      a = TArg . Arg

      fails schema tokens = isLeft (parse schema tokens)
      succeeds schema tokens expected = parse schema tokens == Right expected

  assert $ fails name [o "foo"]
  assert $ fails name []
  assert $ succeeds name [a "foo"] "foo"

  assert $ succeeds nothing [] ()
  assert $ fails nothing [o "foo"]
  assert $ fails nothing [a "foo"]

  assert $ fails large []
  assert $ fails large [o "foo"]
  assert $ fails large [a "foo"]
  assert $ succeeds large [o "large"] Large
  assert $ succeeds large [o "small"] Small

  assert $ fails login []
  assert $ fails login [o "foo"]
  assert $ fails login [a "foo"]
  assert $ fails login [o "username"]
  assert $ fails login [o "username", a "foo"]
  assert $ succeeds login [ o "username", a "foo"
                          , o "password", a "bar"]
              ("foo", "bar")
  assert $ succeeds login [ o "password", a "baz"
                          , o "username", a "quux"]
              ("quux", "baz")

  assert $ succeeds files [] []
  assert $ fails files [o "file"]
  assert $ succeeds files [o "file", a "filename"] ["filename"]

  assert $ fails multiplex []
  assert $ succeeds multiplex [o "codec", a "mp3"] ("mp3", [], [])
  assert $ succeeds multiplex [o "codec", a "mp3"
                              ,o "file", a "file1"
                              ,o "file", a "file1"]
                              ("mp3", ["file1", "file1"], [])
  assert $ succeeds multiplex [o "codec", a "mp3"
                              ,o "file", a "file1"
                              ,o "file", a "file2"
                              ,o "file", a "file1"]
                              ("mp3", ["file1", "file2", "file1"], [])
  assert $ succeeds multiplex [o "codec", a "mp3"
                              ,o "file", a "file1"
                              ,o "output", a "/dev/audio"
                              ,o "file", a "file2"
                              ,o "file", a "file1"]
                              ("mp3", ["file1", "file2", "file1"], ["/dev/audio"])
  assert $ fails multiplex    [o "codec", a "mp3"
                              ,o "file", a "file1"
                              ,o "output"
                              ,o "file", a "file2"
                              ,o "file", a "file1"]

tokenOfString :: String -> Token
tokenOfString s = case s of
  '-':'-':(opt@_) -> TOpt (Opt opt)
  arg          -> TArg (Arg arg)

mainExample :: IO ()
mainExample = do
  args <- System.Environment.getArgs
  let tokenisedArgs = map tokenOfString args
  case parse install tokenisedArgs of
    Left e  -> putStrLn ("error: " ++ e)
    Right r -> print r
