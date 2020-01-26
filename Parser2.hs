{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Category ((>>>))

data ParseResult t a =
    All a
  | Partial (Parser t a)
  | None
  deriving Functor

instance Show a => Show (ParseResult t a) where
  show = \case
    All a     -> "All " ++ show a
    Partial _ -> "Partial"
    None      -> "None"

newtype Parser t a = Parser { stepParser :: [t] -> ([t], ParseResult t a) }
  deriving Functor

runParser :: Parser t a -> [t] -> ([t], Maybe a)
runParser p = stepParser p >>> \case
  (ts', pr) -> case pr of
    All a      -> (ts', Just a)
    Partial p' -> runParser p' ts'
    None       -> (ts', Nothing)

mix :: Parser t (a -> b) -> Parser t a -> Parser t b
mix p1 p2 = Parser (stepParser p1 >>> \case
  (ts, pr1) -> case pr1 of
    All ab      -> (ts, Partial (fmap ab p2))
    Partial p1' -> (ts, Partial (mix p1' p2))
    None        -> case stepParser p2 ts of
      (ts', pr2) -> case pr2 of
        All a       -> (ts', Partial (fmap ($ a) p1))
        Partial p2' -> (ts', Partial (mix p1 p2))
        None        -> (ts', None)
  )

andThen :: Parser t (a -> b) -> Parser t a -> Parser t b
andThen p1 p2 = Parser (stepParser p1 >>> \case
  (ts, pr1) -> case pr1 of
    All ab      -> stepParser (fmap ab p2) ts
    Partial p1' -> stepParser (andThen p1' p2) ts
    None        -> (ts, None)
  )

tokenMaybe :: (t -> Maybe r) -> Parser t r
tokenMaybe f = Parser (\case
  []          -> ([], None)
  ts@(t:rest) -> case f t of
    Just c  -> (rest, All c)
    Nothing -> (ts, None)
  )

satisfy :: (t -> Bool) -> Parser t t
satisfy f = tokenMaybe (\t -> if f t then Just t else Nothing)

string :: Parser t t
string = tokenMaybe Just

switch :: String -> Parser String ()
switch s = fmap (const ()) (satisfy (== ("--" ++ s)))

option :: String -> Parser String String
option s = (flip const <$> switch s) `andThen` string

example :: Parser String (String, String)
example = ((,) <$> option "foo") `mix` option "bar"

examples =
    [ []
    , ["--foo"]
    , ["--foo", "fooarg"]
    , ["--bar", "bararg"]
    , ["--foo", "fooarg", "--bar", "bararg"]
    , ["--bar", "bararg", "--foo", "fooarg"]
    , ["--foo", "--bar", "fooarg", "bararg"]
    ]

main = do
  mapM_ (print . runParser (option "foo")) examples
  mapM_ (print . runParser (option "bar")) examples
  mapM_ (print . runParser example) examples
