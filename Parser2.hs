{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Category ((>>>))
import Data.List

data ParseResult t a =
    All a
  | Partial (Parser t a)
  | None [String]
  deriving Functor

instance Show a => Show (ParseResult t a) where
  show = \case
    All a     -> "All " ++ show a
    Partial _ -> "Partial"
    None s    -> "None " ++ show s

newtype Parser t a = Parser { stepParser :: [t] -> ([t], ParseResult t a) }
  deriving Functor

runParser :: Parser t a -> [t] -> ([t], Either [String] a)
runParser p = stepParser p >>> \case
  (ts', pr) -> case pr of
    All a      -> (ts', Right a)
    Partial p' -> runParser p' ts'
    None s     -> (ts', Left s)

mix :: Parser t (a -> b) -> Parser t a -> Parser t b
mix p1 p2 = Parser (stepParser p1 >>> \case
  (ts, pr1) -> case pr1 of
    All ab      -> (ts, Partial (fmap ab p2))
    Partial p1' -> (ts, Partial (mix p1' p2))
    None s1     -> case stepParser p2 ts of
      (ts', pr2) -> case pr2 of
        All a       -> (ts', Partial (fmap ($ a) p1))
        Partial p2' -> (ts', Partial (mix p1 p2'))
        None s2     -> (ts', None (s1 <> s2))
  )

andThen :: Parser t (a -> b) -> Parser t a -> Parser t b
andThen p1 p2 = Parser (stepParser p1 >>> \case
  (ts, pr1) -> case pr1 of
    All ab      -> stepParser (fmap ab p2) ts
    Partial p1' -> stepParser (andThen p1' p2) ts
    None s      -> (ts, None s)
  )

tokenMaybe :: (t -> Maybe r) -> String -> Parser t r
tokenMaybe f s = Parser (\case
  []          -> ([], None (["End of stream whilst trying to parse " ++ s]))
  ts@(t:rest) -> case f t of
    Just c  -> (rest, All c)
    Nothing -> (ts, None [s])
  )

satisfy :: (t -> Bool) -> String-> Parser t t
satisfy f s = tokenMaybe (\t -> if f t then Just t else Nothing) s

string :: Parser t t
string = tokenMaybe Just "Any token"

switch :: String -> Parser String ()
switch s = fmap (const ()) (satisfy (== ("--" ++ s)) ("--" ++ s))

option :: String -> Parser String String
option s = (flip const <$> switch s) `andThen` string

example :: Parser String (String, String)
example = ((,) <$> option "foo") `mix` option "bar"

example2 = (((,,) <$> option "foo") `mix` option "bar" ) `mix` option "baz"

examples =
    [ []
    , ["--foo"]
    , ["--foo", "fooarg"]
    , ["--bar", "bararg"]
    , ["--foo", "fooarg", "--bar", "bararg"]
    , ["--bar", "bararg", "--foo", "fooarg"]
    , ["--foo", "--bar", "fooarg", "bararg"]
    ]

examples2 = map concat (perms [ [ "--baz", "bazarg" ]
                              , [ "--bar", "bararg" ]
                              , [ "--foo", "fooarg" ]
                              ])

main = do
  flip mapM_ examples $ \ex -> do
    putStr "Input: "
    print ex
    putStr "Result: "
    print (runParser example ex)

  print (runParser example ["--foo","fooarg","--baz","bazarg","--bar","bararg"])
  print (runParser example2 ["--foo","fooarg","--baz","bazarg","--bar","bararg"])

  putStrLn "\nPermutations:\n"

  flip mapM_ examples2 $ \ex ->
    case runParser example2 ex of
      (ts, r) -> case r of
        Left s  -> putStr "Failed : " >> print (ts, ex) >> print s
        Right r -> print (ts, r)

perms [] = [[]]
perms xs = [ x:ps | x <- xs , ps <- perms ( xs\\[x] ) ]
