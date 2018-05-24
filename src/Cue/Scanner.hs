
{- Author: Devin Hill (dhill45@jhu.edu) -}

module Cue.Scanner
  ( Token(..)
  , scan
  ) where

import Parser.Combinators
import Parser.Err

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map




type Scan a = Parser ByteString (Either String) a


data Token
  = Ident ByteString
  | Number Integer
  | Sym'LBRACE
  | Sym'RBRACE
  | Sym'SEMICOLON
  | Sym'LTSIGN
  | Sym'GTSIGN
  | Sym'LTEQUALS
  | Sym'GTEQUALS
  | Sym'EQUALS
  | Sym'NOT
  | Sym'PERCENT
  | Key'GET
  | Key'PUT
  | Key'POP
  | Key'ADD
  | Key'MUL
  | Key'SUB
  | Key'DIV
  | Key'MOD
  | Key'TST
  | Key'CUE
  | Key'DIE
  deriving (Show, Eq, Ord)


  
  -- | A map from bytestrings which would otherwise
--   be valid identifiers to their keyword representation
keywords :: Map ByteString Token
keywords = Map.fromList
  [ "GET" -: Key'GET
  , "PUT" -: Key'PUT
  , "POP" -: Key'POP
  , "ADD" -: Key'ADD
  , "SUB" -: Key'SUB
  , "DIV" -: Key'DIV
  , "MOD" -: Key'MOD
  , "TST" -: Key'TST
  , "CUE" -: Key'CUE
  ]
  where
    (-:) :: a -> b -> (a, b)
    (-:) = (,) -- for fewer parentheses

    
-- | A map of the one-character symbols in SIMPL,
--   the two-character cases are separate (special cases)
symbols :: Map Char Token
symbols = Map.fromList
  [ '%' -: Sym'PERCENT
  , ';' -: Sym'SEMICOLON
  , '=' -: Sym'EQUALS
  , '<' -: Sym'LTSIGN
  , '>' -: Sym'GTSIGN
  , '{' -: Sym'LBRACE
  , '}' -: Sym'RBRACE
  ]    
  where
    (-:) :: a -> b -> (a, b)
    (-:) = (,) -- for fewer parentheses
  
  
  

scan :: Scan [Token]
scan = many (ws *> (identifier <|> number <|> symbol))
  
  

ws :: Scan ()
ws = void $ many . among . map match $
  [ ' ', '\n', '\t', '\r', '\f' ] -- The characters specified to be whitespace


identifier :: Scan Token