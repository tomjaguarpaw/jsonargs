{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wall #-}

import Data.List

data ParseResult t a =
    All [t] a
  | Partial [t] (Parser t a)
  | None [String]
  deriving Functor

instance (Show t, Show a) => Show (ParseResult t a) where
  show = \case
    All ts a     -> "All " ++ show ts ++ show a
    Partial ts _ -> "Partial " ++ show ts
    None s       -> "None " ++ show s

newtype Parser t a = Parser { stepParser :: [t] -> ParseResult t a }
  deriving Functor

runParser :: Parser t a -> [t] -> ([t], Either [String] a)
runParser p ts = case stepParser p ts of
    All ts' a      -> (ts', Right a)
    Partial ts' p' -> runParser p' ts'
    None s         -> (ts, Left s)

mix :: Parser t (a -> b) -> Parser t a -> Parser t b
mix p1 p2 = Parser (\ts -> case stepParser p1 ts of
    All ts' ab      -> Partial ts' (fmap ab p2)
    Partial ts' p1' -> Partial ts' (mix p1' p2)
    None s1     -> case stepParser p2 ts of
        All ts' a       -> Partial ts' (fmap ($ a) p1)
        Partial ts' p2' -> Partial ts' (mix p1 p2')
        None s2         -> None (s1 <> s2)
  )

andThen :: Parser t (a -> b) -> Parser t a -> Parser t b
andThen p1 p2 = Parser (\ts -> case stepParser p1 ts of
    All ts' ab      -> stepParser (fmap ab p2) ts'
    Partial ts' p1' -> stepParser (andThen p1' p2) ts'
    None s          -> None s
  )

tokenMaybe :: (t -> Maybe r) -> String -> Parser t r
tokenMaybe f s = Parser (\case
  []   -> None ["End of stream whilst trying to parse " ++ s]
  t:ts -> case f t of
    Just c  -> All ts c
    Nothing -> None [s]
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

example2 :: Parser String (String, String, String)
example2 = (((,,) <$> option "foo") `mix` option "bar" ) `mix` option "baz"

examples :: [[String]]
examples =
    [ []
    , ["--foo"]
    , ["--foo", "fooarg"]
    , ["--bar", "bararg"]
    , ["--foo", "fooarg", "--bar", "bararg"]
    , ["--bar", "bararg", "--foo", "fooarg"]
    , ["--foo", "--bar", "fooarg", "bararg"]
    ]

examples2 :: [[String]]
examples2 = map concat (perms [ [ "--baz", "bazarg" ]
                              , [ "--bar", "bararg" ]
                              , [ "--foo", "fooarg" ]
                              ])

main :: IO ()
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
        Right rr -> print (ts, rr)

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:ps | x <- xs , ps <- perms ( xs\\[x] ) ]
