

{- Author: Devin Hill (dhill45@jhu.edu) -}
      
module Cue.Parser where

import Parser.Combinators
import Parser.Err
import Parser.Pos

import Control.Exception

import Cue.Scanner



-------------------------------------------------------------------
--    Primary Parser Type                                        --
------------------------------------------------------------------- 

-- | The type of (combinator) parser we'll be using at this stage
type P a = Parser (PState [Token] String) (PErr [WithPos ParseError] IO) a


instance Exception (WithPos ParseError) where
  -- no methods needed
  
  
-------------------------------------------------------------------
--    Parser State Managing type                                 --
------------------------------------------------------------------- 

-- | State the parser carries with it alongside the default
--   token stream state, such as a symbol table and some obverver type `ob`
data PState s = PState
  { state     :: s             -- ^ Contained state we are parsing
  , symbols   :: SymbolTable   -- ^ Symbol table we build up during parsing
  , acc       :: Integer       -- ^ An accumulator for various purposes
  }
  
  
-- | Basic instance of Parsable so that we can still use all of our
--   favorite combinators, with some more complex state in the background.
instance Parsable s a => Parsable (PState s) a where
  next = Parser $ \st@PState{..} -> do
    (n, s') <- runParser next state
    return (n,  st { state = s' })
  
  peek = Parser $ \st@PState{..} -> do
    (p, s') <- runParser peek state
    return (p, st { state = s' })
    
  eof  = Parser $ \st@PState{..} -> do
    ((), s') <- runParser eof state
    return ((), st { state = s' })
    
    
-- | Basic instance of TracksPos for PState so that we can
--   use position combinators as unique tags for identifiers, etc.
instance TracksPos s => TracksPos (PState s a) where
  