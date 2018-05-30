{-# LANGUAGE
      OverloadedStrings
    , MultiParamTypeClasses 
    , FlexibleInstances #-}

{- Author: Devin Hill (98devin@gmail.com) -}

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

import Data.String (fromString)
import Data.Char (toUpper)
import Data.Functor(($>), void)

import Control.Applicative


type Scan a = Parser ByteString Maybe a


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
  | Sym'MINUS
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
  | Key'INC
  | Key'DEC
  deriving (Show, Eq, Ord)


  
-- | A map from bytestrings which would otherwise
--   be valid identifiers to their keyword representation
keywords :: Map ByteString Token
keywords = Map.fromList
  [ "GET" -: Key'GET
  , "PUT" -: Key'PUT
  , "POP" -: Key'POP
  , "ADD" -: Key'ADD
  , "MUL" -: Key'MUL
  , "SUB" -: Key'SUB
  , "DIV" -: Key'DIV
  , "MOD" -: Key'MOD
  , "TST" -: Key'TST
  , "CUE" -: Key'CUE
  , "INC" -: Key'INC
  , "DEC" -: Key'DEC
  , "DIE" -: Key'DIE
  ]
  where
    (-:) :: a -> b -> (a, b)
    (-:) = (,) -- for fewer parentheses

    
-- | A map of the one-character symbols in cue,
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
  , '!' -: Sym'NOT
  , '-' -: Sym'MINUS
  ]    
  where
    (-:) :: a -> b -> (a, b)
    (-:) = (,) -- for fewer parentheses
  
  
    
    
-- Define a parsable interface for ByteStrings to enable combinators
instance Parsable ByteString Char where
  
  next = Parser $ \bstring ->
    case B.uncons bstring of
      Nothing      -> empty
      Just (c, cs) -> pure (c, cs)
      
  peek = Parser $ \bstring ->
    case B.uncons bstring of
      Nothing      -> empty
      Just (c, cs) -> pure (c, bstring)
      
  eof = Parser $ \bstring ->
    if B.null bstring then 
      pure ((), bstring) 
    else 
      empty
      
    
    
  

scan :: Scan [Token]
scan = many (ws *> many (comment *> ws) *> (identifier <|> number <|> symbol)) <* ws <* many (comment *> ws) <* eof
  

digit :: Scan Char
digit = among . map match $ ['0'..'9']
  

letter :: Scan Char
letter = among . map match $ "_" ++ ['A'..'Z'] ++ ['a'..'z']


-- general whitespace
ws :: Scan ()
ws = void $ many . among . map match $
  [ ' ', '\n', '\t', '\r', '\f' ] -- The characters specified to be whitespace
  
  
comment :: Scan ()
comment = do
  match '#'
  many $ satisfy (not . (`elem` ("\r\n" :: String))) next
  void (match '\n') <|> void (matches "\r\n")
  return ()
  

identifier :: Scan Token
identifier = do
  str <- fromString <$> ((:) <$> letter <*> many (digit <|> letter))
  return $ Map.findWithDefault (Ident str) (B.map toUpper str) keywords
  

number :: Scan Token
number = Number . read <$> some digit


symbol :: Scan Token
symbol = do
  chr <- among . map match $ "{};%=<>!-"
  
  -- if it could be part of a multi-char symbol,
  -- then test the next character to see if it is.
  case chr of
    '<' -> (match '=' $> Sym'LTEQUALS) <|> pure Sym'LTSIGN
    '>' -> (match '=' $> Sym'GTEQUALS) <|> pure Sym'GTSIGN
    -- otherwise, proceed as usual, fetching
    -- the token this symbol represents from our mapping.
    chr -> pure $ (Map.!) symbols chr