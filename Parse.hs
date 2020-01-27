{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

import Control.Category ((>>>))

data ParseResult t a =
    Complete a
  | Partial (Parser t a)
  | None
  deriving Functor

instance Show a => Show (ParseResult t a) where
  show = \case
    Complete a -> "Complete " ++ show a
    Partial _ -> "Partial"
    None -> "None"

newtype Parser t a = Parser { runParser :: [t] -> ([t], ParseResult t a) }
  deriving Functor

mix :: Parser t (a -> b) -> Parser t a -> Parser t b
mix p1 p2 = Parser (runParser p1 >>> \case
  (ts, pr1) -> case pr1 of
    Complete ab -> runParser (fmap ab p2) ts
    Partial p1' -> runParser (mix p1' p2) ts
    None        -> case runParser p2 ts of
      (ts', pr2) -> case pr2 of
        Complete a  -> runParser (fmap ($ a) p1) ts'
        Partial p2' -> runParser (mix p1 p2') ts'
        None        -> (ts', None)
  )

andThen :: Parser t (a -> b) -> Parser t a -> Parser t b
andThen p1 p2 = Parser (runParser p1 >>> \case
  (ts, pr1) -> case pr1 of
    Complete ab -> runParser (fmap ab p2) ts
    Partial p1' -> runParser (p1' `andThen` p2) ts
    None        -> (ts, None)
  )

instance Applicative (Parser t) where
  pure a  = Parser (\t -> (t, Complete a))
  (<*>) = mix

token :: Parser t t
token = tokenLike Just

tokenLike :: (t -> Maybe r) -> Parser t r
tokenLike f = Parser (\case
  []          -> ([], None)
  ts@(t:rest) -> case f t of
    Just r  -> (rest, Complete r)
    Nothing -> (ts, None)
  )

satisfy :: (t -> Bool) -> Parser t t
satisfy f = tokenLike (\t -> if f t then Just t else Nothing)

option :: String -> Parser String String
option s = (const id <$> satisfy (== ("--" ++ s))) `andThen` token

example = ((,) <$> option "foo") `mix` option "bar"

main :: IO ()
main = do
  mapM_ (print . runParser example) [ []
                                    , [ "--foo", "fooarg", "--bar", "bararg" ]
                                    , [ "--bar", "bararg", "--foo", "fooarg" ]
                                    , [ "--bar", "--foo", "bararg", "fooarg" ]
                                    ]
