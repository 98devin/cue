{-# LANGUAGE 
      LambdaCase
    , FlexibleInstances
    , MultiParamTypeClasses
    , FunctionalDependencies
    , FlexibleContexts
    , ScopedTypeVariables #-}

{- Author: Devin Hill (98devin@gmail.com) -}
    
module Parser.Combinators
  ( Parser(..)
  , evalParser
  , execParser
  , compose
  , mapParser
  
  -- Parsable class for more generic parsers --
  , Parsable (next, peek, eof)
  
  -- Various combinators --
  , exactly
  , asManyAs
  , satisfy
  , sepBy
  , sepBy'
  , among
  , match
  , matches
  ) where

import Control.Applicative  
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Arrow (first)

import Control.Exception


-------------------------------------------------------------------
--    Primary Parser type and important helpers                  --
-------------------------------------------------------------------

-- | A parser which transforms a structure of type s
--   into a result of type a, with potential extra states represented
--   using a monadic type m. Yes, this is pretty much StateT, by accident.
newtype Parser s m a = Parser { runParser :: s -> m (a, s) } 
    
-- | Run the parser to completion, extracting its returned value
evalParser :: Functor m => Parser s m a -> s -> m a
evalParser p s = fst <$> runParser p s

-- | Run the parser to completion, extracting the final state of the input
execParser :: Functor m => Parser s m a -> s -> m s
execParser p s = snd <$> runParser p s



-------------------------------------------------------------------
--    Instance Declarations                                      --
-------------------------------------------------------------------

instance Functor f => Functor (Parser s f) where
  fmap f p = Parser $ fmap (first f) . runParser p
  
  
instance Monad m => Applicative (Parser s m) where
  pure a = Parser $ \s -> pure (a, s)
  
  p1 <*> p2 = Parser $ \s -> do 
    (f, s')  <- runParser p1 s
    (x, s'') <- runParser p2 s'
    return (f x, s'')
    
    
instance Monad m => Monad (Parser s m) where
  return = pure
  
  p1 >>= f = Parser $ \s -> do
    (a, s') <- runParser p1 s
    runParser (f a) s'
    
    
instance (Monad m, Alternative m) => Alternative (Parser s m) where
  empty = Parser $ const empty
  
  p1 <|> p2 = Parser $ \s -> 
    runParser p1 s <|> runParser p2 s


instance (Monad m, Alternative m) => MonadPlus (Parser s m) where
  -- use defaults
  

instance MonadIO m => MonadIO (Parser s m) where
  liftIO io = Parser $ \s -> do
    a <- liftIO io
    return (a, s)


instance Fail.MonadFail m => Fail.MonadFail (Parser s m) where
  fail msg = Parser $ \_ -> Fail.fail msg
    
    
-------------------------------------------------------------------
--    Parsable class, for abstracting over the parsed type       --
-------------------------------------------------------------------
    
-- | Class representing that we can trivially extract elements
--   of type `b` from a source `a` in order to make a parser
class Parsable a b | a -> b where
  
  -- | Take the next token unconditionally from the input
  next :: (Monad m, Alternative m) => Parser a m b
  
  -- | Look at the next token, but do not advance past it
  peek :: (Monad m, Alternative m) => Parser a m b
  
  -- | Test whether the input has ended. Should fail if the input is not over.
  eof :: (Monad m, Alternative m) => Parser a m ()
  
  
-- | The most obvious intstance of Parsable, and
--   useful, being usable for String, [Token], etc.
instance Parsable [a] a where
  next = Parser $ \case
    []     -> empty
    (c:cs) -> pure (c,   cs)
      
  peek = Parser $ \case
    []     -> empty
    (c:cs) -> pure (c, c:cs)
    
  eof = Parser $ \case
    [] -> pure ((), [])
    _  -> empty    

    

-------------------------------------------------------------------
--    Basic Combinator Functions                                 --
-------------------------------------------------------------------

-- | Feed the results of one parser directly into the
--   input of another; this ignores any unconsumed input
--   the second parser has left behind.
compose :: Monad m => Parser s m a -> Parser a m b -> Parser s m b
compose p1 p2 = Parser $ \s -> do
  (a, s') <- runParser p1 s
  (b, a') <- runParser p2 a
  return (b, s')
  
-- | Turn a parser of one kind into another,
--   using a function which can map between two output types
mapParser :: (Monad m, Monad n) => (m (a, s) -> n (b, s)) -> Parser s m a -> Parser s n b 
mapParser mn p = Parser $ \s -> mn $ runParser p s

  
-- | Parse "exactly" n copies of a construct, and collect them into a list
exactly :: (Monad m, Alternative m) => Int -> Parser s m a -> Parser s m [a]
exactly 0 _ = pure []
exactly n p = (:) <$> p <*> exactly (n - 1) p
  

-- | Parse up to and including n copies of a construct,
--   and collect them into a list
asManyAs :: (Monad m, Alternative m) => Int -> Parser s m a -> Parser s m [a]
asManyAs 0 _ = pure []
asManyAs n p = (:) <$> p <*> asManyAs (n - 1) p <|> pure []
  
  
-- | Produce a parser which will only accept if its output
--   also satisfies a provided predicate
satisfy :: (Monad m, Alternative m) => (a -> Bool) -> Parser s m a -> Parser s m a
satisfy pred p = do
  a <- p
  guard $ pred a -- prevent success upon failure of the predicate
  return a
  

-- | Produce a parser which accepts 0 or more copies of p separated by `sep`
sepBy :: (Monad m, Alternative m) => Parser s m b -> Parser s m a -> Parser s m [a]
sepBy sep p = sepBy' sep p <|> pure []  


-- | Produce a parser which accepts 1 or more copies of p separated by `sep`
sepBy' :: (Monad m, Alternative m) => Parser s m b -> Parser s m a -> Parser s m [a]
sepBy' sep p = (:) <$> p <*> many (sep *> p)


-- | Produce a parser which accepts whenever one of the provided options accepts.
among :: (Monad m, Alternative m) => [Parser s m a] -> Parser s m a
among = foldl (<|>) empty


-- | Parse a single primitive element from some parsable stream of data
match :: (Monad m, Alternative m, Parsable a b, Eq b) => b -> Parser a m b
match b = satisfy (== b) next


-- | Parse a sequence of primitive elements in order from some parsable stream of data
matches :: (Monad m, Alternative m, Parsable a b, Eq b) => [b] -> Parser a m [b]
matches = foldr (\c -> (<*>) ((:) <$> match c)) (pure [])

-- A more sane implementation, but
-- my linter keeps wanting to refactor it to the above
-- so I just yielded to its desires eventually.
{-
  matches []     = pure []
  matches (c:cs) = (:) <$> match c <*> matches cs
-}
