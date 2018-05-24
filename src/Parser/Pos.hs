{-# LANGUAGE
      MultiParamTypeClasses 
    , FlexibleContexts
    , FlexibleInstances
    , UndecidableInstances #-}
    
{- Author: Devin Hill (dhill45@jhu.edu) -}
    
module Parser.Pos
  ( Pos(..)
  , PosRange(..)
  , WithPos(..)
  , WithRange(..)
  , withInitPos
  
  , TracksPos(..)
  
  , withPos
  , withRange
  
  ) where
  
import Parser.Combinators

import Control.Applicative
  


-------------------------------------------------------------------
--    Parsing which also tracks position                         --
-------------------------------------------------------------------
  
-- | Type representing the current position of a parser
--   as it moves through some string-like data structure
data Pos = Pos
  { lineNumber    :: !Int
  , linePosition  :: !Int
  , charNumber    :: !Int
  }
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos ln lp cn) = show ln ++ ":" ++ show lp


-- | Data representing a range bounded by two positions 
data PosRange = PosRange
  { initPos  :: !Pos
  , finalPos :: !Pos
  }
  deriving (Eq, Ord)

instance Show PosRange where
  show (PosRange p1 p2) = show p1 ++ "-" ++ show p2


-- | An abstraction representing a data type which includes
--   not only a parsable value but also a position within it,
--   represented as line, character within line, and absolute character counts.  
data WithPos a = WithPos !Pos a

instance Show a => Show (WithPos a) where
  show (WithPos pos a) =
    show a ++ 
      " at (" ++ show pos ++ ")" 


-- | Similarly, a datatype which records the first and last positions
--   in the text which were used to produce the value `a`.
data WithRange a = WithRange !PosRange a

instance Show a => Show (WithRange a) where
  show (WithRange rng a) =
    show a ++
      " at (" ++ show rng ++ ")"

    

-- | Create a WithPos object by starting at position zero
withInitPos :: a -> WithPos a
withInitPos = WithPos (Pos 1 1 0)


-- | Anything which we can parse into `Char`s normally,
--   we can parse into Char WHILE keeping track of our position
--   by line and character within the line, etc.
instance Parsable s Char => Parsable (WithPos s) Char where

  -- Just delegate to the normal `next`, but
  -- also keep track of when newlines appear to adjust
  -- the state of our position tracking accordingly
  next = Parser $ \(WithPos (Pos ln lp cn) s) -> do
    (char, s') <- runParser next s
    let nextPos = case char of
          '\n' -> WithPos (Pos (ln + 1) 1        (cn + 1)) s' -- advance line if newline, and reset line pos
          _    -> WithPos (Pos ln       (lp + 1) (cn + 1)) s' -- otherwise, increment line pos
    return (char, nextPos)
  
  -- Simply fetches the character without updating the position
  peek = Parser $ \(WithPos pos s) -> do
    (char, s') <- runParser peek s
    return (char, WithPos pos s')
  
  -- This is just a trivial delegation to the
  -- other `eof` but with a new output state type.
  eof = Parser $ \(WithPos pos s) -> do
    ((), s') <- runParser eof s
    return ((), WithPos pos s')
    
    
    
-------------------------------------------------------------------
--    Combinators to assist position usage                       --
-------------------------------------------------------------------
    
-- | Class representing the types of state which carry positional
--   information within them (or can get it somehow), so that parsers
--   can use this information for errors, values, etc.
--   Minimal implementation: position || (lineNo && linePos && charNo)
class TracksPos s where

  -- | Fetch the current position in its entirety
  position :: (Monad m, Alternative m) => Parser s m Pos
  position = Pos <$> lineNo <*> linePos <*> charNo
  
  -- | Fetch the current line number
  lineNo :: (Monad m, Alternative m) => Parser s m Int
  lineNo = lineNumber <$> position

  -- | Fetch the current line position
  linePos :: (Monad m, Alternative m) => Parser s m Int
  linePos = linePosition <$> position

  -- | Fetch the current overall character position
  charNo :: (Monad m, Alternative m) => Parser s m Int
  charNo = charNumber <$> position


-- -- | The instance for position tracking in case our state carries a position directly
instance TracksPos (WithPos s) where
  
  position = Parser $ \st@(WithPos pos s) -> return (pos, st)
  
  -- Other methods will use the default implementation
  
  
-- -- | The simplest general instance of TrackPos, when our state can produce positioned tokens
-- instance Parsable s (WithPos a) => TracksPos s where
  
--   position = Parser $ \s -> do
--     (WithPos pos _, s') <- runParser peek s
--     return (pos, s')

--   -- Other methods will use the default implementation
  
  
  
-- | Modify an existing parser to also return positional information
--   alongside its existing result
withPos :: (TracksPos s, Monad m, Alternative m) => Parser s m a -> Parser s m (WithPos a)
withPos p = do
  pos <- position
  WithPos pos <$> p

  
-- | Modify an existing parser to also return RANGED positional information
--   alongside its existing result
withRange :: (TracksPos s, Monad m, Alternative m) => Parser s m a -> Parser s m (WithRange a)
withRange p = do
  initPos <- position
  a <- p
  endPos  <- position
  return $ WithRange (PosRange initPos endPos) a
